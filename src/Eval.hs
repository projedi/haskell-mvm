module Eval
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
  }

emptyEnv :: Env
emptyEnv =
  Env
    { envLayers = []
    , envForeignFunctions = IntMap.empty
    , envFunctions = IntMap.empty
    , envConsts = IntMap.empty
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

readConstantFromEnv :: Env -> ConstID -> Maybe Value
readConstantFromEnv env (ConstID cid) = IntMap.lookup cid (envConsts env)

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

splitLayers :: LayerID -> Execute [Layer]
splitLayers lid = do
  env@Env {envLayers = ls} <- State.get
  let (newenv, preserve) = List.splitAt lid $ List.reverse ls
  State.put $ env {envLayers = List.reverse newenv}
  pure $ List.reverse preserve

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

printCall :: [Value] -> Execute ()
printCall vals =
  Trans.liftIO $ do
    str <- concat <$> mapM showIO vals
    putStr str

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
  -> [Value]
  -> ForeignFun
  -> Execute (Maybe Value)
foreignFunctionCall rettype params vals fun =
  Trans.liftIO $ call fun rettype (assertVals params vals)
  where
    assertVals [] [] = []
    assertVals (vtype:ps) (v:vs)
      | typeIs v vtype = v : assertVals ps vs
    assertVals _ _ = error "Type mismatch"

functionCall :: FunctionCall -> Execute (Maybe Value)
functionCall (PrintCall args) = do
  vals <- evaluateArgs args
  printCall vals
  pure Nothing
functionCall ForeignFunctionCall { foreignFunCallName = FunID fid
                                 , foreignFunCallArgs = args
                                 } = do
  vals <- evaluateArgs args
  Just (fdecl, f) <- State.gets (IntMap.lookup fid . envForeignFunctions)
  foreignFunctionCall
    (foreignFunDeclRetType fdecl)
    (foreignFunDeclParams fdecl)
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

readConstant :: ConstID -> Execute Value
readConstant name = do
  env <- State.get
  let Just val = readConstantFromEnv env name
  pure val

evaluate :: Expr -> Execute Value
evaluate (ExprFunctionCall fcall) = do
  Just val <- functionCall fcall
  pure val
evaluate (ExprVar _ vname) = readVariable vname
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

evaluateAsBool :: Expr -> Execute Bool
evaluateAsBool e = do
  i <- evaluateAsInt e
  pure $ i /= 0

evaluateAsInt :: Expr -> Execute Int
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
execute (StatementIfElse e btrue bfalse) = do
  res <- evaluateAsBool e
  if res
    then executeBlock btrue
    else executeBlock bfalse
execute (StatementReturn Nothing) = functionReturn Nothing
execute (StatementReturn (Just e)) = do
  res <- evaluate e
  functionReturn (Just res)
