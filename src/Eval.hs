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
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.List as List

import ASMSyntax
import ForeignEval
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

data Env = Env
  { envForeignFunctions :: IntMap (ForeignFunctionDecl, ForeignFun)
  , envFunctions :: IntMap FunctionDef
  , envConsts :: IntMap Value
  , envVarTypes :: IntMap VarType
  , envRIP :: Int
  , envRSP :: Int
  , envStack :: [Value]
  , envVarMap :: IntMap Int -- displacements to RBP.
  , envRBP :: Int
  }

emptyEnv :: Env
emptyEnv =
  Env
    { envForeignFunctions = IntMap.empty
    , envFunctions = IntMap.empty
    , envConsts = IntMap.empty
    , envVarTypes = IntMap.empty
    , envRIP = 0
    , envRSP = 0
    , envStack = []
    , envVarMap = IntMap.empty
    , envRBP = 0
    }

readVariableFromEnv :: Env -> VarID -> Value
readVariableFromEnv env (VarID vid) =
  let d = (envVarMap env) IntMap.! vid
   in (envStack env) !! (envRBP env + d)

writeVariableToEnv :: Env -> VarID -> Value -> Env
writeVariableToEnv env (VarID vid) val =
  let d = (envVarMap env) IntMap.! vid
      (before, _:after) = List.splitAt (envRBP env + d) $ envStack env
   in env {envStack = before ++ [val] ++ after}

readConstantFromEnv :: Env -> ConstID -> Value
readConstantFromEnv env (ConstID cid) = (envConsts env) IntMap.! cid

dereferenceInEnv :: Env -> Value -> Value
dereferenceInEnv env (ValuePtr _ [d]) = (envStack env) !! d
dereferenceInEnv _ _ = error "Type mismatch"

addressOfInEnv :: Env -> VarID -> Value
addressOfInEnv env (VarID vid) =
  let d = (envVarMap env) IntMap.! vid
   in ValuePtr (envVarTypes env IntMap.! vid) [d + envRBP env]

writeToPtrInEnv :: Env -> Value -> Value -> Env
writeToPtrInEnv env (ValuePtr _ [d]) val =
  let (stackBefore, _:stackAfter) = List.splitAt d $ envStack env
   in env {envStack = stackBefore ++ [val] ++ stackAfter}
writeToPtrInEnv _ _ _ = error "Type mismatch"

type Execute = ExceptT (Maybe Value) (StateT Env IO)

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

pushOnStack :: Value -> Execute ()
pushOnStack v =
  State.modify $ \env ->
    env {envRSP = envRSP env + 1, envStack = envStack env ++ [v]}

popFromStack :: Execute Value
popFromStack = do
  env <- State.get
  let (before, [target]) =
        List.splitAt (length (envStack env) - 1) (envStack env)
  State.put $ env {envRSP = envRSP env - 1, envStack = before}
  pure target

declareVariable :: Var -> Execute ()
declareVariable (Var (VarID vid) vtype) = do
  State.modify $ \env ->
    env
      {envVarMap = IntMap.insert vid (envRSP env - envRBP env) $ envVarMap env}
  pushOnStack (defaultValueFromType vtype)

undeclareVariable :: Var -> Execute ()
undeclareVariable _ = popFromStack >> pure ()

nativeFunctionCall :: FunctionDef -> [Value] -> Execute (Maybe Value)
nativeFunctionCall fdef vals = do
  res <-
    do rbp <- State.gets envRBP
       pushOnStack (ValueInt $ fromIntegral rbp)
       rsp <- State.gets envRSP
       State.modify $ \env -> env {envRBP = rsp}
       let allLocals = funDefParams fdef ++ funDefLocals fdef
       mapM_ declareVariable allLocals
       generateAssignments (funDefParams fdef) vals
       mv <- executeFunctionBody (funDefBody fdef)
       mapM_ undeclareVariable (reverse allLocals)
       ValueInt oldRBP <- popFromStack
       State.modify $ \env -> env {envRBP = fromIntegral oldRBP}
       pure mv
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

generateAssignments :: [Var] -> [Value] -> Execute ()
generateAssignments [] [] = pure ()
generateAssignments (v:vs) (val:vals) = do
  writeVariable v val
  generateAssignments vs vals
generateAssignments _ _ = error "Type mismatch"

evaluateArgs :: [Var] -> Execute [Value]
evaluateArgs = mapM readVariable

executeFunctionBody :: [Statement] -> Execute (Maybe Value)
executeFunctionBody [] = pure Nothing
executeFunctionBody ss = do
  prevIP <- State.gets envRIP
  State.modify $ \env -> env {envRIP = 0}
  let instructions = Array.listArray (0, length ss - 1) ss
  retValue <-
    (startExecution instructions >> pure Nothing) `Except.catchError` pure
  State.modify (\env -> env {envRIP = prevIP})
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

readVariable :: Var -> Execute Value
readVariable v = State.gets $ \env -> readVariableFromEnv env (varName v)

writeVariable :: Var -> Value -> Execute ()
writeVariable v val =
  State.modify $ \env -> writeVariableToEnv env (varName v) val

readConstant :: ConstID -> Execute Value
readConstant name = State.gets $ \env -> readConstantFromEnv env name

addressOf :: Var -> Execute Value
addressOf v = State.gets $ \env -> addressOfInEnv env (varName v)

dereference :: Value -> Execute Value
dereference ptr = State.gets $ \env -> dereferenceInEnv env ptr

writeToPtr :: Value -> Value -> Execute ()
writeToPtr ptr val = State.modify $ \env -> writeToPtrInEnv env ptr val

evaluateUnOp :: UnOp -> (Value -> Value)
evaluateUnOp UnNeg = negate
evaluateUnOp UnNot =
  \v ->
    case v of
      ValueInt 0 -> ValueInt 1
      ValueInt _ -> ValueInt 0
      _ -> error "Type mismatch"
evaluateUnOp UnIntToFloat =
  \v ->
    case v of
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
evaluate (ExprVar v) = readVariable v
evaluate (ExprAddressOf v) = addressOf v
evaluate (ExprDereference p) = do
  v <- readVariable p
  dereference v
evaluate (ExprConst _ c) = readConstant c
evaluate (ExprUnOp op v) = evaluateUnOp op <$> readVariable v
evaluate (ExprBinOp op vl vr) =
  evaluateBinOp op <$> readVariable vl <*> readVariable vr

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
  State.modify $ \env -> env {envRIP = ip - 1}

executeStatement :: ExecuteStatement ()
executeStatement = do
  ipBefore <- State.gets envRIP
  arr <- Reader.asks constEnvInstructions
  execute (arr Array.! ipBefore)
  ipAfter <- State.gets envRIP
  let nextIP = ipAfter + 1
  when (nextIP <= snd (Array.bounds arr)) $ do
    State.modify $ \env -> env {envRIP = nextIP}
    executeStatement

execute :: Statement -> ExecuteStatement ()
execute (StatementFunctionCall fcall) =
  Trans.lift (functionCall fcall) >> pure ()
execute (StatementAssign var e) = do
  res <- Trans.lift $ evaluate e
  Trans.lift $ writeVariable var res
execute (StatementAssignToPtr ptr var) = do
  res <- Trans.lift $ readVariable var
  v <- Trans.lift $ readVariable ptr
  Trans.lift $ writeToPtr v res
execute (StatementReturn Nothing) = Trans.lift $ functionReturn Nothing
execute (StatementReturn (Just v)) = do
  res <- Trans.lift $ readVariable v
  Trans.lift $ functionReturn (Just res)
execute (StatementLabel _) = pure ()
execute (StatementJump l) = jump l
execute (StatementJumpIfZero v l) = do
  res <- Trans.lift $ valueAsBool <$> readVariable v
  unless res $ jump l
