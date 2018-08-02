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
import Data.Int (Int64)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.List as List

import ASMSyntax
import ForeignEval
import Value

typesMatch :: VarType -> VarType -> Bool
typesMatch lhs rhs
  | lhs == rhs = True
typesMatch VarTypeInt (VarTypePtr _) = True
typesMatch (VarTypePtr _) VarTypeInt = True
typesMatch _ _ = False

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
  forM_ libhandles dlclose

data Env = Env
  { envForeignFunctions :: IntMap (ForeignFunctionDecl, ForeignFun)
  , envFunctions :: IntMap FunctionDef
  , envConsts :: IntMap Value
  , envRIP :: Int
  , envStack :: [Value]
  , regRBP :: Int64
  , regRSP :: Int64
  }

instance Show Env where
  show env =
    "rip = " ++ show (envRIP env) ++
    ", rbp = " ++ show (regRBP env) ++
    ", rsp = " ++ show (regRSP env) ++
    ", stack = " ++ show (envStack env)

emptyEnv :: Env
emptyEnv =
  Env
    { envForeignFunctions = IntMap.empty
    , envFunctions = IntMap.empty
    , envConsts = IntMap.empty
    , envRIP = 0
    , envStack = []
    , regRBP = 0
    , regRSP = 0
    }

type Execute = ExceptT (Maybe Value) (StateT Env IO)

runExecute :: Execute () -> IO ()
runExecute m = do
  _ <- runStateT (runExceptT m) emptyEnv
  pure ()

startExecute ::
     IntMap ForeignFunctionDecl
  -> IntMap FunctionDef
  -> IntMap Value
  -> Execute ()
startExecute foreignFuns nativeFuns consts = do
  funs <- mapM getForeignFun foreignFuns
  State.modify $ \env ->
    env
      { envForeignFunctions = funs
      , envFunctions = nativeFuns
      , envConsts = consts
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
    env {regRSP = regRSP env + 1, envStack = envStack env ++ [v]}

popFromStack :: VarType -> Execute ()
popFromStack t = do
  v <- State.gets (List.last . envStack)
  unless (typesMatch (typeof v) t) $ error "Type mismatch"
  State.modify $ \env ->
    env {regRSP = regRSP env - 1, envStack = List.init (envStack env)}

nativeFunctionCall :: FunctionDef -> [Value] -> Execute (Maybe Value)
nativeFunctionCall fdef vals = do
  res <-
    do Nothing <- executeFunctionBody (funDefBeforeBody fdef)
       generateAssignments (funDefParams fdef) vals
       mv <- executeFunctionBody (funDefBody fdef)
       Nothing <- executeFunctionBody (funDefAfterBody fdef)
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
generateAssignments (Var {varType = t, varDisplacement = d}:vs) (val:vals) = do
  writePointer
    Pointer
      {pointerType = t, pointerBase = Just RegisterRBP, pointerDisplacement = d}
    val
  generateAssignments vs vals
generateAssignments _ _ = error "Type mismatch"

evaluateArgs :: [Operand] -> Execute [Value]
evaluateArgs = mapM readOperand

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

readConstant :: ConstID -> Execute Value
readConstant (ConstID cid) = State.gets $ ((IntMap.! cid) . envConsts)

dereference :: Value -> Execute Value
dereference (ValueInt d) = State.gets ((!! (fromIntegral d)) . envStack)
dereference _ = error "Type mismatch"

writeToPtr :: Value -> Value -> Execute ()
writeToPtr (ValueInt d) val = do
  (before, target:after) <-
    State.gets (List.splitAt (fromIntegral d) . envStack)
  unless (typesMatch (typeof target) (typeof val)) $ error "Type mismatch"
  State.modify $ \env -> env {envStack = before ++ [val] ++ after}
writeToPtr _ _ = error "Type mismatch"

readRegister :: Register -> Execute Value
readRegister RegisterRSP = State.gets (ValueInt . regRSP)
readRegister RegisterRBP = State.gets (ValueInt . regRBP)

writeRegister :: Register -> Value -> Execute ()
writeRegister RegisterRSP (ValueInt i) = State.modify $ \env -> env {regRSP = i}
writeRegister RegisterRSP _ = error "Type mismatch"
writeRegister RegisterRBP (ValueInt i) = State.modify $ \env -> env {regRBP = i}
writeRegister RegisterRBP _ = error "Type mismatch"

readPointer :: Pointer -> Execute Value
readPointer Pointer {pointerBase = mr, pointerDisplacement = d} = do
  b <- maybe (pure $ ValueInt 0) readRegister mr
  dereference $ b + ValueInt d

writePointer :: Pointer -> Value -> Execute ()
writePointer Pointer {pointerBase = mr, pointerDisplacement = d} val = do
  b <- maybe (pure $ ValueInt 0) readRegister mr
  writeToPtr (b + ValueInt d) val

readOperand :: Operand -> Execute Value
readOperand (OperandRegister _ r) = readRegister r
readOperand (OperandPointer p) = readPointer p
readOperand (OperandImmediateInt i) = pure $ ValueInt i

writeOperand :: Operand -> Value -> Execute ()
writeOperand (OperandRegister _ r) val = writeRegister r val
writeOperand (OperandPointer p) val = writePointer p val
writeOperand (OperandImmediateInt _) _ = error "Type mismatch"

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
evaluate (ExprRead x) = readOperand x
evaluate (ExprDereference p) = do
  v <- readOperand p
  dereference v
evaluate (ExprConst _ c) = readConstant c
evaluate (ExprUnOp op x) = evaluateUnOp op <$> readOperand x
evaluate (ExprBinOp op lhs rhs) =
  evaluateBinOp op <$> readOperand lhs <*> readOperand rhs

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
execute (StatementAssign lhs e) = do
  res <- Trans.lift $ evaluate e
  Trans.lift $ writeOperand lhs res
execute (StatementAssignToPtr ptr rhs) = do
  res <- Trans.lift $ readOperand rhs
  v <- Trans.lift $ readOperand ptr
  Trans.lift $ writeToPtr v res
execute (StatementPushOnStack x) = do
  res <- Trans.lift $ readOperand x
  Trans.lift $ pushOnStack res
execute (StatementAllocateOnStack t) = do
  Trans.lift $ pushOnStack (defaultValueFromType t)
execute (StatementPopFromStack t) = Trans.lift $ popFromStack t
execute (StatementReturn Nothing) = Trans.lift $ functionReturn Nothing
execute (StatementReturn (Just x)) = do
  res <- Trans.lift $ readOperand x
  Trans.lift $ functionReturn (Just res)
execute (StatementLabel _) = pure ()
execute (StatementJump l) = jump l
execute (StatementJumpIfZero x l) = do
  res <- Trans.lift $ valueAsBool <$> readOperand x
  unless res $ jump l
