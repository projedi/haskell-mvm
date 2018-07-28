module Eval
  ( eval
  ) where

import Control.Monad (forM, forM_, unless, when)
import Control.Monad.Except (ExceptT, runExceptT)
import qualified Control.Monad.Except as Except
import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Reader as Reader
import Control.Monad.State (StateT, runStateT)
import qualified Control.Monad.State as State
import qualified Control.Monad.Trans as Trans
import Data.Array (Array)
import qualified Data.Array as Array
import Data.Bits
import Data.Foldable (asum)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.List as List

import ForeignEval
import LinearSyntax
import Value

eval :: Program -> IO ()
eval p = do
  libhandles <-
    forM (programLibraries p) $ \l -> do
      Right h <- dlopen l
      pure h
  runExecute $
    startExecute
      (programForeignFunctions p)
      (programFunctions p)
      (programConstants p)
      (programVariables p)
  forM_ libhandles dlclose

type LayerID = Int

newtype Layer = Layer
  { varEnv :: IntMap Value
  }

emptyLayer :: Layer
emptyLayer = Layer {varEnv = IntMap.empty}

readVariableFromLayer :: Layer -> VarID -> Maybe Value
readVariableFromLayer l (VarID vid) = IntMap.lookup vid (varEnv l)

writeVariableToLayer :: Layer -> VarID -> Value -> Maybe Layer
writeVariableToLayer l (VarID vid) value =
  let (moldvalue, newvarenv) =
        IntMap.insertLookupWithKey (\_ a _ -> a) vid value (varEnv l)
   in case moldvalue of
        Just oldValue
          | typeIs value (typeof oldValue) -> Just $ l {varEnv = newvarenv}
        _ -> Nothing

addVariableToLayer :: Layer -> VarID -> VarType -> Maybe Layer
addVariableToLayer l (VarID vid) vtype =
  let (oldvalue, newvarenv) =
        IntMap.insertLookupWithKey
          (\_ a _ -> a)
          vid
          (defaultValueFromType vtype)
          (varEnv l)
   in maybe (Just $ l {varEnv = newvarenv}) (const Nothing) oldvalue

data Env = Env
  { envLayers :: [Layer]
  , envForeignFunctions :: IntMap (ForeignFunctionDecl, ForeignFun)
  , envFunctions :: IntMap FunctionDef
  , envConsts :: IntMap Value
  , envVarTypes :: IntMap VarType
  , envInstructionPointer :: Int
  }

emptyEnv :: Env
emptyEnv =
  Env
    { envLayers = []
    , envForeignFunctions = IntMap.empty
    , envFunctions = IntMap.empty
    , envConsts = IntMap.empty
    , envVarTypes = IntMap.empty
    , envInstructionPointer = 0
    }

modifyingLayer :: (Layer -> Maybe Layer) -> [Layer] -> Maybe [Layer]
modifyingLayer _ [] = Nothing
modifyingLayer f (l:ls) =
  case f l of
    Nothing -> (l :) <$> modifyingLayer f ls
    Just l' -> Just (l' : ls)

readVariableFromEnv :: Env -> VarID -> Maybe Value
readVariableFromEnv Env {envLayers = curlayer:otherlayers} vname =
  asum (map (`readVariableFromLayer` vname) (curlayer : otherlayers))
readVariableFromEnv _ _ = error "Env broke"

writeVariableToEnv :: Env -> VarID -> Value -> Maybe Env
writeVariableToEnv env@Env {envLayers = curlayer:otherlayers} vname val =
  case modifyingLayer
         (\l -> writeVariableToLayer l vname val)
         (curlayer : otherlayers) of
    Just (curlayer':otherlayers') ->
      Just (env {envLayers = curlayer' : otherlayers'})
    Just _ -> error "Impossible"
    Nothing -> Nothing
writeVariableToEnv _ _ _ = error "Env broke"

addVariableToEnv :: Env -> VarID -> VarType -> Maybe Env
addVariableToEnv env@Env {envLayers = curlayer:otherlayers} vname vtype =
  case addVariableToLayer curlayer vname vtype of
    Nothing -> Nothing
    Just l' -> Just (env {envLayers = l' : otherlayers})
addVariableToEnv _ _ _ = error "Env broke"

readConstantFromEnv :: Env -> ConstID -> Maybe Value
readConstantFromEnv env (ConstID cid) = IntMap.lookup cid (envConsts env)

splitLayersInEnv :: Env -> LayerID -> ([Layer], Layer, [Layer])
splitLayersInEnv Env {envLayers = ls} lid =
  let (before, target:after) = List.splitAt lid $ List.reverse ls
   in (List.reverse after, target, List.reverse before)

dereferenceInEnv :: Env -> Value -> Maybe Value
dereferenceInEnv env (ValuePtr _ [lid, vid]) =
  let (_, target, _) = splitLayersInEnv env lid
   in readVariableFromLayer target (VarID vid)
dereferenceInEnv _ _ = error "Type mismatch"

addressOfInEnv :: Env -> VarID -> Maybe Value
addressOfInEnv Env {envLayers = layers} name@(VarID vid) = go layers
  where
    go [] = Nothing
    go (l:ls) =
      case readVariableFromLayer l name of
        Nothing -> go ls
        Just v -> Just $ ValuePtr (typeof v) [length ls, vid]

writeToPtrInEnv :: Env -> Value -> Value -> Maybe Env
writeToPtrInEnv env (ValuePtr _ [lid, vid]) val =
  let (after, target, before) = splitLayersInEnv env lid
   in case writeVariableToLayer target (VarID vid) val of
        Nothing -> Nothing
        Just target' -> Just $ env {envLayers = after ++ [target'] ++ before}
writeToPtrInEnv _ _ _ = error "Type mismatch"

type Execute = ExceptT (Maybe Value) (StateT Env IO)

withNewLayer :: Execute a -> Execute a
withNewLayer m = do
  lid <- currentLayer
  newLayer
  res <- m
  dropLayer
  newLid <- currentLayer
  when (lid /= newLid) $ error "Scoping broke."
  pure res
  where
    newLayer =
      State.modify
        (\env@Env {envLayers = layers} -> env {envLayers = emptyLayer : layers})
    dropLayer =
      State.modify
        (\env@Env {envLayers = layers} -> env {envLayers = tail layers})

currentLayer :: Execute LayerID
currentLayer = State.gets (length . envLayers)

runExecute :: Execute () -> IO ()
runExecute m = do
  _ <- runStateT (runExceptT m) emptyEnv
  pure ()

startExecute ::
     IntMap ForeignFunctionDecl
  -> IntMap FunctionDef
  -> IntMap Value
  -> IntMap VarType
  -> Execute ()
startExecute foreignFuns nativeFuns consts vars = do
  funs <- mapM getForeignFun foreignFuns
  State.modify $ \env ->
    env
      { envForeignFunctions = funs
      , envFunctions = nativeFuns
      , envConsts = consts
      , envVarTypes = vars
      }
  let Just mainFun = IntMap.lookup 0 nativeFuns
  _ <- nativeFunctionCall mainFun []
  pure ()
  where
    getForeignFun fdecl = do
      Just f <- Trans.liftIO $ findSymbol $ foreignFunDeclRealName fdecl
      pure (fdecl, f)

declareVariable :: VarDecl -> Execute ()
declareVariable (VarDecl vtype vname) = do
  env <- State.get
  let Just env' = addVariableToEnv env vname vtype
  State.put env'

nativeFunctionCall :: FunctionDef -> [Value] -> Execute (Maybe Value)
nativeFunctionCall fdef vals = do
  res <-
    withNewLayer $ do
      generateAssignments (funDefParams fdef) vals
      mapM_ generateLocal (funDefLocals fdef)
      executeFunctionBody (funDefBody fdef)
  case (res, funDefRetType fdef) of
    (Nothing, Nothing) -> pure res
    (Just val, Just valtype)
      | typeIs val valtype -> pure $ Just val
    _ -> error "Type mismatch"

foreignFunctionCall ::
     Maybe VarType
  -> [VarType]
  -> Bool
  -> [Value]
  -> ForeignFun
  -> Execute (Maybe Value)
foreignFunctionCall rettype params hasVarArgs vals fun =
  Trans.liftIO $ call fun rettype (assertVals params vals)
  where
    assertVals [] [] = []
    assertVals (vtype:ps) (v:vs)
      | typeIs v vtype = v : assertVals ps vs
    assertVals [] vs@(_:_)
      | hasVarArgs = vs
    assertVals _ _ = error "Type mismatch"

functionCall :: FunctionCall -> Execute (Maybe Value)
functionCall ForeignFunctionCall { foreignFunCallName = FunID fid
                                 , foreignFunCallArgs = args
                                 } = do
  vals <- evaluateArgs args
  Just (fdecl, f) <- State.gets (IntMap.lookup fid . envForeignFunctions)
  foreignFunctionCall
    (foreignFunDeclRetType fdecl)
    (foreignFunDeclParams fdecl)
    (foreignFunDeclHasVarArgs fdecl)
    vals
    f
functionCall NativeFunctionCall { nativeFunCallName = (FunID fid)
                                , nativeFunCallArgs = args
                                } = do
  vals <- evaluateArgs args
  Just f <- State.gets (IntMap.lookup fid . envFunctions)
  nativeFunctionCall f vals

generateAssignment :: VarDecl -> Value -> Execute ()
generateAssignment decl@(VarDecl _ name) val = do
  declareVariable decl
  writeVariable name val

generateAssignments :: [VarDecl] -> [Value] -> Execute ()
generateAssignments [] [] = pure ()
generateAssignments (decl:decls) (val:vals) = do
  generateAssignment decl val
  generateAssignments decls vals
generateAssignments _ _ = error "Type mismatch"

generateLocal :: VarID -> Execute ()
generateLocal (VarID v) = do
  Just vt <- State.gets (IntMap.lookup v . envVarTypes)
  declareVariable (VarDecl vt (VarID v))

evaluateArgs :: [Expr] -> Execute [Value]
evaluateArgs = mapM evaluate

executeFunctionBody :: [Statement] -> Execute (Maybe Value)
executeFunctionBody [] = pure Nothing
executeFunctionBody ss = do
  prevIP <- State.gets envInstructionPointer
  State.modify $ \env -> env {envInstructionPointer = 0}
  let instructions = Array.listArray (0, length ss - 1) ss
  retValue <-
    (startExecution instructions >> pure Nothing) `Except.catchError` pure
  State.modify (\env -> env {envInstructionPointer = prevIP})
  pure retValue
  where
    startExecution instructions =
      runReaderT executeStatement $
      ConstEnv
        { constEnvInstructions = instructions
        , constEnvLabelMap = buildLabelMap $ Array.assocs instructions
        }
    buildLabelMap [] = IntMap.empty
    buildLabelMap ((i, StatementLabel (LabelID lid)):is) =
      IntMap.insert lid i $ buildLabelMap is
    buildLabelMap (_:is) = buildLabelMap is

functionReturn :: Maybe Value -> Execute ()
functionReturn = Except.throwError

readVariable :: VarID -> Execute Value
readVariable name = do
  env <- State.get
  let Just val = readVariableFromEnv env name
  pure val

writeVariable :: VarID -> Value -> Execute ()
writeVariable name val = do
  env <- State.get
  let Just env' = writeVariableToEnv env name val
  State.put env'

readConstant :: ConstID -> Execute Value
readConstant name = do
  env <- State.get
  let Just val = readConstantFromEnv env name
  pure val

addressOf :: VarID -> Execute Value
addressOf name = do
  env <- State.get
  let Just val = addressOfInEnv env name
  pure val

dereference :: Value -> Execute Value
dereference ptr = do
  env <- State.get
  let Just val = dereferenceInEnv env ptr
  pure val

writeToPtr :: Value -> Value -> Execute ()
writeToPtr ptr val = do
  env <- State.get
  let Just env' = writeToPtrInEnv env ptr val
  State.put env'

evaluate :: Expr -> Execute Value
evaluate (ExprFunctionCall fcall) = do
  Just val <- functionCall fcall
  pure val
evaluate (ExprVar _ vname) = readVariable vname
evaluate (ExprAddressOf _ vname) = addressOf vname
evaluate (ExprDereference _ vname) = do
  v <- readVariable vname
  dereference v
evaluate (ExprConst _ c) = readConstant c
evaluate (ExprUnOp UnNeg e) = negate <$> evaluate e
evaluate (ExprBinOp BinPlus el er) = (+) <$> evaluate el <*> evaluate er
evaluate (ExprBinOp BinMinus el er) = (-) <$> evaluate el <*> evaluate er
evaluate (ExprBinOp BinTimes el er) = (*) <$> evaluate el <*> evaluate er
evaluate (ExprBinOp BinDiv el er) = (/) <$> evaluate el <*> evaluate er
evaluate (ExprBinOp BinMod el er) = rem <$> evaluate el <*> evaluate er
evaluate (ExprBinOp BinBitAnd el er) = (.&.) <$> evaluate el <*> evaluate er
evaluate (ExprBinOp BinBitOr el er) = (.|.) <$> evaluate el <*> evaluate er
evaluate (ExprBinOp BinBitXor el er) = xor <$> evaluate el <*> evaluate er
evaluate (ExprUnOp UnNot e) = do
  val <- evaluate e
  case val of
    ValueInt 0 -> pure $ ValueInt 1
    ValueInt _ -> pure $ ValueInt 0
    _ -> error "Type mismatch"
evaluate (ExprBinOp BinAnd el er) =
  (\lhs rhs -> fromBool (toBool lhs && toBool rhs)) <$> evaluate el <*>
  evaluate er
evaluate (ExprBinOp BinOr el er) =
  (\lhs rhs -> fromBool (toBool lhs || toBool rhs)) <$> evaluate el <*>
  evaluate er
evaluate (ExprBinOp BinEq el er) =
  (fromBool .) . (==) <$> evaluate el <*> evaluate er
evaluate (ExprBinOp BinLt el er) =
  (fromBool .) . (<) <$> evaluate el <*> evaluate er
evaluate (ExprUnOp UnIntToFloat e) = go <$> evaluate e
  where
    go (ValueInt i) = ValueFloat $ fromIntegral i
    go _ = error "Type mismatch"

valueAsBool :: Value -> Bool
valueAsBool (ValueInt i) = i /= 0
valueAsBool _ = error "Type mismatch"

data ConstEnv = ConstEnv
  { constEnvInstructions :: Array Int Statement
  , constEnvLabelMap :: IntMap Int
  }

type ExecuteStatement = ReaderT ConstEnv Execute

jump :: LabelID -> ExecuteStatement ()
jump (LabelID lid) = do
  ip <- Reader.asks ((IntMap.! lid) . constEnvLabelMap)
  State.modify $ \env -> env {envInstructionPointer = ip - 1}

executeStatement :: ExecuteStatement ()
executeStatement = do
  ipBefore <- State.gets envInstructionPointer
  arr <- Reader.asks constEnvInstructions
  execute (arr Array.! ipBefore)
  ipAfter <- State.gets envInstructionPointer
  let nextIP = ipAfter + 1
  when (nextIP <= snd (Array.bounds arr)) $ do
    State.modify $ \env -> env {envInstructionPointer = nextIP}
    executeStatement

execute :: Statement -> ExecuteStatement ()
execute (StatementFunctionCall fcall) =
  Trans.lift (functionCall fcall) >> pure ()
execute (StatementAssign var e) = do
  res <- Trans.lift $ evaluate e
  Trans.lift $ writeVariable var res
execute (StatementAssignToPtr ptr var) = do
  res <- Trans.lift $ readVariable $ varName var
  v <- Trans.lift $ readVariable ptr
  Trans.lift $ writeToPtr v res
execute (StatementReturn Nothing) = Trans.lift $ functionReturn Nothing
execute (StatementReturn (Just e)) = do
  res <- Trans.lift $ evaluate e
  Trans.lift $ functionReturn (Just res)
execute (StatementLabel _) = pure ()
execute (StatementJump l) = jump l
execute (StatementJumpIfZero v l) = do
  res <- Trans.lift $ valueAsBool <$> readVariable (varName v)
  unless res $ jump l
