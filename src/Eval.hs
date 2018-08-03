module Eval
  ( eval
  ) where

import Control.Monad (forM, forM_, unless, when)
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
import CallingConvention (CallingConvention)
import qualified CallingConvention
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
  , envStack :: [Value]
  , regRIP :: Int
  , regRBP :: Int64
  , regRSP :: Int64
  , regRAX :: Value
  , regRDI :: Value
  , regRSI :: Value
  , regRDX :: Value
  , regRCX :: Value
  , regR8 :: Value
  , regR9 :: Value
  , regXMM0 :: Double
  , regXMM1 :: Double
  , regXMM2 :: Double
  , regXMM3 :: Double
  , regXMM4 :: Double
  , regXMM5 :: Double
  , regXMM6 :: Double
  , regXMM7 :: Double
  }

instance Show Env where
  show env =
    "rip = " ++
    show (regRIP env) ++
    ", rbp = " ++
    show (regRBP env) ++
    ", rsp = " ++ show (regRSP env) ++ ", stack = " ++ show (envStack env)

emptyEnv :: Env
emptyEnv =
  Env
    { envForeignFunctions = IntMap.empty
    , envFunctions = IntMap.empty
    , envConsts = IntMap.empty
    , regRIP = 0
    , envStack = []
    , regRBP = 0
    , regRSP = 0
    , regRAX = ValueInt 0
    , regRDI = ValueInt 0
    , regRSI = ValueInt 0
    , regRDX = ValueInt 0
    , regRCX = ValueInt 0
    , regR8 = ValueInt 0
    , regR9 = ValueInt 0
    , regXMM0 = 0
    , regXMM1 = 0
    , regXMM2 = 0
    , regXMM3 = 0
    , regXMM4 = 0
    , regXMM5 = 0
    , regXMM6 = 0
    , regXMM7 = 0
    }

type Execute = StateT Env IO

runExecute :: Execute () -> IO ()
runExecute m = do
  _ <- runStateT m emptyEnv
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
  _ <- nativeFunctionCall mainFun
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

prepareArgsAtCall :: CallingConvention -> Execute [Value]
prepareArgsAtCall cc = do
  mapM go (CallingConvention.funArgValues cc)
  where
    go :: CallingConvention.ArgLocation -> Execute Value
    go (CallingConvention.ArgLocationRegister _ r) = readRegister r
    go (CallingConvention.ArgLocationStack t d) =
      readPointer
        Pointer
          { pointerType = t
          , pointerBase = Just RegisterRBP
          , pointerDisplacement = -(d + 3)
          }

nativeFunctionCall :: FunctionDef -> Execute ()
nativeFunctionCall fdef = do
  ip <- State.gets regRIP
  pushOnStack (ValueInt $ fromIntegral ip)
  executeFunctionBody (funDefBody fdef)

foreignFunctionCall ::
     Maybe VarType -> [VarType] -> Bool -> [VarType] -> ForeignFun -> Execute ()
foreignFunctionCall rettype params hasVarArgs args fun
  -- We need to model what happens during nativeFunctionCAll because of prepareArgsAtCall
 = do
  let cc =
        CallingConvention.computeCallingConvention
          CallingConvention.FunctionCall
            { CallingConvention.funRetType = rettype
            , CallingConvention.funArgTypes = args
            }
  ip <- State.gets regRIP
  pushOnStack (ValueInt $ fromIntegral ip)
  rbp1 <- readRegister RegisterRBP
  pushOnStack rbp1
  rsp1 <- readRegister RegisterRSP
  writeRegister RegisterRBP rsp1
  vals <- prepareArgsAtCall cc
  res <- Trans.liftIO $ call fun rettype (assertVals params vals)
  rbp2 <-
    readPointer $
    Pointer
      { pointerType = VarTypeInt
      , pointerBase = Just RegisterRSP
      , pointerDisplacement = -1
      }
  writeRegister RegisterRBP rbp2
  popFromStack VarTypeInt
  popFromStack VarTypeInt
  case (res, CallingConvention.funRetValue cc) of
    (Nothing, Nothing) -> pure ()
    (Just r, Just (_, rv)) -> writeRegister rv r
    _ -> error "Type mismatch"
  where
    assertVals [] [] = []
    assertVals (vtype:ps) (v:vs)
      | typeIs v vtype = v : assertVals ps vs
    assertVals [] vs@(_:_)
      | hasVarArgs = vs
    assertVals _ _ = error "Type mismatch"

functionCall :: FunctionCall -> Execute ()
functionCall ForeignFunctionCall { foreignFunCallName = FunID fid
                                 , foreignFunCallArgTypes = args
                                 } = do
  Just (fdecl, f) <- State.gets (IntMap.lookup fid . envForeignFunctions)
  foreignFunctionCall
    (foreignFunDeclRetType fdecl)
    (foreignFunDeclParams fdecl)
    (foreignFunDeclHasVarArgs fdecl)
    args
    f
functionCall NativeFunctionCall {nativeFunCallName = (FunID fid)} = do
  Just f <- State.gets (IntMap.lookup fid . envFunctions)
  nativeFunctionCall f

executeFunctionBody :: [Statement] -> Execute ()
executeFunctionBody [] = pure ()
executeFunctionBody ss = do
  prevIP <- State.gets regRIP
  State.modify $ \env -> env {regRIP = 0}
  let instructions = Array.listArray (0, length ss - 1) ss
  startExecution instructions
  State.modify (\env -> env {regRIP = prevIP})
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
readRegister RegisterRAX = State.gets regRAX
readRegister RegisterRDI = State.gets regRDI
readRegister RegisterRSI = State.gets regRSI
readRegister RegisterRDX = State.gets regRDX
readRegister RegisterRCX = State.gets regRCX
readRegister RegisterR8 = State.gets regR8
readRegister RegisterR9 = State.gets regR9
readRegister RegisterXMM0 = State.gets (ValueFloat . regXMM0)
readRegister RegisterXMM1 = State.gets (ValueFloat . regXMM1)
readRegister RegisterXMM2 = State.gets (ValueFloat . regXMM2)
readRegister RegisterXMM3 = State.gets (ValueFloat . regXMM3)
readRegister RegisterXMM4 = State.gets (ValueFloat . regXMM4)
readRegister RegisterXMM5 = State.gets (ValueFloat . regXMM5)
readRegister RegisterXMM6 = State.gets (ValueFloat . regXMM6)
readRegister RegisterXMM7 = State.gets (ValueFloat . regXMM7)

writeRegister :: Register -> Value -> Execute ()
writeRegister RegisterRSP (ValueInt i) = State.modify $ \env -> env {regRSP = i}
writeRegister RegisterRSP _ = error "Type mismatch"
writeRegister RegisterRBP (ValueInt i) = State.modify $ \env -> env {regRBP = i}
writeRegister RegisterRBP _ = error "Type mismatch"
writeRegister RegisterRAX v = State.modify $ \env -> env {regRAX = v}
writeRegister RegisterRDI v = State.modify $ \env -> env {regRDI = v}
writeRegister RegisterRSI v = State.modify $ \env -> env {regRSI = v}
writeRegister RegisterRDX v = State.modify $ \env -> env {regRDX = v}
writeRegister RegisterRCX v = State.modify $ \env -> env {regRCX = v}
writeRegister RegisterR8 v = State.modify $ \env -> env {regR8 = v}
writeRegister RegisterR9 v = State.modify $ \env -> env {regR9 = v}
writeRegister RegisterXMM0 (ValueFloat f) =
  State.modify $ \env -> env {regXMM0 = f}
writeRegister RegisterXMM0 _ = error "Type mismatch"
writeRegister RegisterXMM1 (ValueFloat f) =
  State.modify $ \env -> env {regXMM1 = f}
writeRegister RegisterXMM1 _ = error "Type mismatch"
writeRegister RegisterXMM2 (ValueFloat f) =
  State.modify $ \env -> env {regXMM2 = f}
writeRegister RegisterXMM2 _ = error "Type mismatch"
writeRegister RegisterXMM3 (ValueFloat f) =
  State.modify $ \env -> env {regXMM3 = f}
writeRegister RegisterXMM3 _ = error "Type mismatch"
writeRegister RegisterXMM4 (ValueFloat f) =
  State.modify $ \env -> env {regXMM4 = f}
writeRegister RegisterXMM4 _ = error "Type mismatch"
writeRegister RegisterXMM5 (ValueFloat f) =
  State.modify $ \env -> env {regXMM5 = f}
writeRegister RegisterXMM5 _ = error "Type mismatch"
writeRegister RegisterXMM6 (ValueFloat f) =
  State.modify $ \env -> env {regXMM6 = f}
writeRegister RegisterXMM6 _ = error "Type mismatch"
writeRegister RegisterXMM7 (ValueFloat f) =
  State.modify $ \env -> env {regXMM7 = f}
writeRegister RegisterXMM7 _ = error "Type mismatch"

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
  State.modify $ \env -> env {regRIP = ip - 1}

executeStatement :: ExecuteStatement ()
executeStatement = do
  ipBefore <- State.gets regRIP
  arr <- Reader.asks constEnvInstructions
  execute (arr Array.! ipBefore)
  ipAfter <- State.gets regRIP
  let nextIP = ipAfter + 1
  when (nextIP <= snd (Array.bounds arr)) $ do
    State.modify $ \env -> env {regRIP = nextIP}
    executeStatement

functionReturn :: ExecuteStatement ()
functionReturn = do
  Trans.lift $ popFromStack VarTypeInt -- popping RIP
  -- TODO: Jump to RIP.

execute :: Statement -> ExecuteStatement ()
execute (StatementFunctionCall fcall) = Trans.lift $ functionCall fcall
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
execute StatementReturn = functionReturn
execute (StatementLabel _) = pure ()
execute (StatementJump l) = jump l
execute (StatementJumpIfZero x l) = do
  res <- Trans.lift $ valueAsBool <$> readOperand x
  unless res $ jump l
