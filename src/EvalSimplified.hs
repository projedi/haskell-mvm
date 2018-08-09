{-# LANGUAGE LambdaCase #-}

module EvalSimplified
  ( eval
  ) where

import Control.Monad (forM, forM_, when)
import Control.Monad.Except (ExceptT, runExceptT)
import qualified Control.Monad.Except as Except
import Control.Monad.State (StateT, runStateT)
import qualified Control.Monad.State as State
import qualified Control.Monad.Trans as Trans
import Data.Bits
import Data.Foldable (asum)
import Data.Int (Int64)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.List as List

import ForeignEval
import SimplifiedSyntax
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
      (programStrings p)
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
  , envStrings :: IntMap String
  , envVarTypes :: IntMap VarType
  }

emptyEnv :: Env
emptyEnv =
  Env
    { envLayers = []
    , envForeignFunctions = IntMap.empty
    , envFunctions = IntMap.empty
    , envStrings = IntMap.empty
    , envVarTypes = IntMap.empty
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

type Execute a = ExceptT (Maybe Value) (StateT Env IO) a

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

splitLayers :: LayerID -> Execute ()
splitLayers lid = do
  env <- State.get
  let (_, _, newenv) = splitLayersInEnv env lid
  State.put $ env {envLayers = newenv}

runExecute :: Execute () -> IO ()
runExecute m = do
  _ <- runStateT (runExceptT m) emptyEnv
  pure ()

startExecute ::
     IntMap ForeignFunctionDecl
  -> IntMap FunctionDef
  -> IntMap String
  -> IntMap VarType
  -> Execute ()
startExecute foreignFuns nativeFuns strings vars = do
  funs <- mapM getForeignFun foreignFuns
  State.modify $ \env ->
    env
      { envForeignFunctions = funs
      , envFunctions = nativeFuns
      , envStrings = strings
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
      executeBlockWithReturn (funDefBody fdef)
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

evaluateArgs :: [Expr] -> Execute [Value]
evaluateArgs = mapM evaluate

executeBlockWithReturn :: Block -> Execute (Maybe Value)
executeBlockWithReturn block = do
  lid <- currentLayer
  (executeBlock block >> pure Nothing) `Except.catchError` restoreFromReturn lid
  where
    restoreFromReturn lid val = do
      _ <- splitLayers lid
      pure val

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

readImmediate :: Immediate -> Execute Value
readImmediate (ImmediateInt i) = pure $ ValueInt i
readImmediate (ImmediateFloat f) = pure $ ValueFloat f
readImmediate (ImmediateString (StringID sid)) = do
  val <- State.gets ((IntMap.! sid) . envStrings)
  pure $ ValueString $ Right val

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

evaluateUnOp :: UnOp -> (Value -> Value)
evaluateUnOp UnNeg = negate
evaluateUnOp UnNot =
  \case
    ValueInt 0 -> ValueInt 1
    ValueInt _ -> ValueInt 0
    _ -> error "Type mismatch"
evaluateUnOp UnIntToFloat =
  \case
    ValueInt i -> ValueFloat $ fromIntegral i
    _ -> error "Type mismatch"

evaluateBinOp :: BinOp -> (Value -> Value -> Value)
evaluateBinOp BinPlus = (+)
evaluateBinOp BinMinus = (-)
evaluateBinOp BinTimes = (*)
evaluateBinOp BinDiv = (/)
evaluateBinOp BinMod = rem
evaluateBinOp BinBitAnd = (.&.)
evaluateBinOp BinBitOr = (.|.)
evaluateBinOp BinBitXor = xor
evaluateBinOp BinAnd = \lhs rhs -> fromBool (toBool lhs && toBool rhs)
evaluateBinOp BinOr = \lhs rhs -> fromBool (toBool lhs || toBool rhs)
evaluateBinOp BinEq = (fromBool .) . (==)
evaluateBinOp BinLt = (fromBool .) . (<)

evaluate :: Expr -> Execute Value
evaluate (ExprFunctionCall fcall) = do
  Just val <- functionCall fcall
  pure val
evaluate (ExprVar _ vname) = readVariable vname
evaluate (ExprAddressOf _ vname) = addressOf vname
evaluate (ExprDereference _ vname) = do
  v <- readVariable vname
  dereference v
evaluate (ExprConst imm) = readImmediate imm
evaluate (ExprUnOp op e) = evaluateUnOp op <$> evaluate e
evaluate (ExprBinOp op el er) = evaluateBinOp op <$> evaluate el <*> evaluate er

evaluateAsBool :: Expr -> Execute Bool
evaluateAsBool e = do
  i <- evaluateAsInt e
  pure $ i /= 0

evaluateAsInt :: Expr -> Execute Int64
evaluateAsInt e = do
  ValueInt i <- evaluate e
  pure i

executeBlock :: Block -> Execute ()
executeBlock block = withNewLayer $ forM_ (blockStatements block) execute

execute :: Statement -> Execute ()
execute (StatementBlock block) = executeBlock block
execute (StatementVarAlloc (VarID v)) = do
  Just vt <- State.gets (IntMap.lookup v . envVarTypes)
  declareVariable (VarDecl vt (VarID v))
execute (StatementFunctionCall fcall) = functionCall fcall >> pure ()
execute s@(StatementWhile e block) = do
  res <- evaluateAsBool e
  when res $ do
    executeBlock block
    execute s
execute (StatementAssign var e) = do
  res <- evaluate e
  writeVariable var res
execute (StatementAssignToPtr var e) = do
  res <- evaluate e
  v <- readVariable var
  writeToPtr v res
execute (StatementIfElse e btrue bfalse) = do
  res <- evaluateAsBool e
  if res
    then executeBlock btrue
    else executeBlock bfalse
execute (StatementReturn Nothing) = functionReturn Nothing
execute (StatementReturn (Just e)) = do
  res <- evaluate e
  functionReturn (Just res)
