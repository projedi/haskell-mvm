module EvalASM
  ( eval
  ) where

import Control.Monad (forM, forM_, unless, when)
import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Reader as Reader
import Control.Monad.State (StateT, evalStateT)
import qualified Control.Monad.State as State
import qualified Control.Monad.Trans as Trans
import Data.Array (Array)
import qualified Data.Array as Array
import Data.Array.IO (IOArray)
import qualified Data.Array.IO as IOArray
import Data.Bits
import Data.Int (Int64)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import ASMSyntax
import CallingConvention (CallingConvention)
import qualified CallingConvention
import ForeignEval
import Value

-- In reality all our types are 8 bytes long, so our stack addresses at 8 bytes
stackScale :: Int64
stackScale = 8

-- Stack size in bytes.
stackSize :: Int64
stackSize = 80000000

typesMatch :: VarType -> VarType -> Bool
typesMatch lhs rhs
  | lhs == rhs = True
typesMatch VarTypeInt (VarTypePtr _) = True
typesMatch (VarTypePtr _) VarTypeInt = True
typesMatch _ _ = False

eval :: Program -> IO ()
eval Program {programCode = []} = pure ()
eval p = do
  libhandles <-
    forM (programLibraries p) $ \l -> do
      Right h <- dlopen l
      pure h
  funs <-
    forM (programForeignFunctions p) $ \fdecl -> do
      Just f <- Trans.liftIO $ findSymbol $ foreignFunDeclRealName fdecl
      pure (fdecl, f)
  let code =
        Array.listArray
          (0, fromIntegral $ length (programCode p) - 1)
          (programCode p)
  stack <- IOArray.newArray_ (0, stackSize `div` stackScale)
  evalStateT
    (runReaderT
       executeCode
       ConstEnv
         { constEnvInstructions = code
         , constEnvLabelMap = buildLabelMap $ Array.assocs code
         , constEnvForeignFunctions = funs
         , constEnvStrings = programStrings p
         , constEnvStack = stack
         })
    emptyEnv
  forM_ libhandles dlclose
  where
    buildLabelMap [] = IntMap.empty
    buildLabelMap ((i, InstructionLabelledNOP (LabelID lid)):is) =
      IntMap.insert lid i $ buildLabelMap is
    buildLabelMap (_:is) = buildLabelMap is

data EFLAGS = EFLAGS
  { efZF :: Bool
  , efSF :: Bool
  , efCF :: Bool
  }

data Env = Env
  { regRIP :: Int64
  , regRBP :: Int64
  , regRSP :: Int64
  , regRAX :: Value
  , regRBX :: Value
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
  , regEFLAGS :: EFLAGS
  }

emptyEnv :: Env
emptyEnv =
  Env
    { regRIP = 0
    , regRBP = 0
    , regRSP = stackSize
    , regRAX = ValueInt 0
    , regRBX = ValueInt 0
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
    , regEFLAGS = EFLAGS {efZF = False, efSF = False, efCF = False}
    }

data ConstEnv = ConstEnv
  { constEnvInstructions :: Array Int64 Instruction
  , constEnvLabelMap :: IntMap Int64
  , constEnvForeignFunctions :: IntMap (ForeignFunctionDecl, ForeignFun)
  , constEnvStrings :: IntMap String
  , constEnvStack :: IOArray Int64 Value
  }

type Execute = ReaderT ConstEnv (StateT Env IO)

executeCode :: Execute ()
executeCode = do
  pushOnStack (ValueInt specialRIPValue)
  go
  where
    specialRIPValue = -42
    go = do
      ipBefore <- State.gets regRIP
      arr <- Reader.asks constEnvInstructions
      execute (arr Array.! ipBefore)
      ipAfter <- State.gets regRIP
      let nextIP = ipAfter + 1
      State.modify $ \env -> env {regRIP = nextIP}
      unless (nextIP == specialRIPValue + 1) go

readFromStack :: Int64 -> Execute Value
readFromStack d = do
  stack <- Reader.asks constEnvStack
  Trans.liftIO $ IOArray.readArray stack (d `div` stackScale)

writeToStack :: Int64 -> Value -> Execute ()
writeToStack d val = do
  stack <- Reader.asks constEnvStack
  Trans.liftIO $ IOArray.writeArray stack (d `div` stackScale) val

pushOnStack :: Value -> Execute ()
pushOnStack v = do
  d <- State.gets regRSP
  let d' = d - typeSize (typeof v)
  writeToStack d' v
  State.modify $ \env -> env {regRSP = d'}

popFromStack :: VarType -> Execute Value
popFromStack t = do
  d <- State.gets regRSP
  v <- readFromStack d
  unless (typesMatch (typeof v) t) $ error "Type mismatch"
  State.modify $ \env -> env {regRSP = d + typeSize t}
  pure v

prepareArgsAtCall :: CallingConvention -> Execute [Value]
prepareArgsAtCall cc = mapM go (CallingConvention.funArgValues cc)
  where
    go :: CallingConvention.ArgLocation -> Execute Value
    go (CallingConvention.ArgLocationRegister _ r) = readRegister r
    go (CallingConvention.ArgLocationRegisterXMM r) = readRegisterXMM r
    go (CallingConvention.ArgLocationStack t d) =
      readPointer
        Pointer
          { pointerType = t
          , pointerBase = RegisterRBP
          , pointerDisplacement = d + 2 * typeSize VarTypeInt
          }

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
  pushOnStack (ValueInt ip)
  rbp1 <- readRegister RegisterRBP
  pushOnStack rbp1
  rsp1 <- readRegister RegisterRSP
  writeRegister RegisterRBP rsp1
  vals <- prepareArgsAtCall cc
  res <- Trans.liftIO $ call fun rettype (assertVals params vals)
  rbp2 <- popFromStack VarTypeInt
  writeRegister RegisterRBP rbp2
  _ <- popFromStack VarTypeInt -- Popping RIP
  case (res, CallingConvention.funRetValue cc) of
    (Nothing, Nothing) -> pure ()
    (Just r, Just (CallingConvention.RetLocationRegister _ rv)) ->
      writeRegister rv r
    (Just r, Just (CallingConvention.RetLocationRegisterXMM rv)) ->
      writeRegisterXMM rv r
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
  Just (fdecl, f) <- Reader.asks (IntMap.lookup fid . constEnvForeignFunctions)
  foreignFunctionCall
    (foreignFunDeclRetType fdecl)
    (foreignFunDeclParams fdecl)
    (foreignFunDeclHasVarArgs fdecl)
    args
    f
functionCall NativeFunctionCall {nativeFunCallName = lbl} = do
  ip <- State.gets regRIP
  pushOnStack (ValueInt ip)
  jump lbl

readImmediate :: Immediate -> Execute Value
readImmediate (ImmediateInt i) = pure $ ValueInt i
readImmediate (ImmediateFloat f) = pure $ ValueFloat f

readRegister :: Register -> Execute Value
readRegister RegisterRSP = State.gets (ValueInt . regRSP)
readRegister RegisterRBP = State.gets (ValueInt . regRBP)
readRegister RegisterRAX = State.gets regRAX
readRegister RegisterRBX = State.gets regRBX
readRegister RegisterRDI = State.gets regRDI
readRegister RegisterRSI = State.gets regRSI
readRegister RegisterRDX = State.gets regRDX
readRegister RegisterRCX = State.gets regRCX
readRegister RegisterR8 = State.gets regR8
readRegister RegisterR9 = State.gets regR9

readRegisterXMM :: RegisterXMM -> Execute Value
readRegisterXMM RegisterXMM0 = State.gets (ValueFloat . regXMM0)
readRegisterXMM RegisterXMM1 = State.gets (ValueFloat . regXMM1)
readRegisterXMM RegisterXMM2 = State.gets (ValueFloat . regXMM2)
readRegisterXMM RegisterXMM3 = State.gets (ValueFloat . regXMM3)
readRegisterXMM RegisterXMM4 = State.gets (ValueFloat . regXMM4)
readRegisterXMM RegisterXMM5 = State.gets (ValueFloat . regXMM5)
readRegisterXMM RegisterXMM6 = State.gets (ValueFloat . regXMM6)
readRegisterXMM RegisterXMM7 = State.gets (ValueFloat . regXMM7)

writeRegister :: Register -> Value -> Execute ()
writeRegister RegisterRSP (ValueInt i) = State.modify $ \env -> env {regRSP = i}
writeRegister RegisterRSP _ = error "Type mismatch"
writeRegister RegisterRBP (ValueInt i) = State.modify $ \env -> env {regRBP = i}
writeRegister RegisterRBP _ = error "Type mismatch"
writeRegister RegisterRAX v = State.modify $ \env -> env {regRAX = v}
writeRegister RegisterRBX v = State.modify $ \env -> env {regRBX = v}
writeRegister RegisterRDI v = State.modify $ \env -> env {regRDI = v}
writeRegister RegisterRSI v = State.modify $ \env -> env {regRSI = v}
writeRegister RegisterRDX v = State.modify $ \env -> env {regRDX = v}
writeRegister RegisterRCX v = State.modify $ \env -> env {regRCX = v}
writeRegister RegisterR8 v = State.modify $ \env -> env {regR8 = v}
writeRegister RegisterR9 v = State.modify $ \env -> env {regR9 = v}

writeLowByte :: Value -> Value -> Value
writeLowByte (ValueInt byte) (ValueInt originalValue) =
  ValueInt $ (originalValue `div` 0xff) * 0xff + (byte `rem` 0xff)
writeLowByte _ _ = error "Type mismatch"

writeRegister8 :: Register8 -> Value -> Execute ()
writeRegister8 RegisterAL v =
  State.modify $ \env -> env {regRAX = writeLowByte v (regRAX env)}
writeRegister8 RegisterCL v =
  State.modify $ \env -> env {regRCX = writeLowByte v (regRCX env)}
writeRegister8 RegisterDL v =
  State.modify $ \env -> env {regRDX = writeLowByte v (regRDX env)}

writeRegisterXMM :: RegisterXMM -> Value -> Execute ()
writeRegisterXMM RegisterXMM0 (ValueFloat f) =
  State.modify $ \env -> env {regXMM0 = f}
writeRegisterXMM RegisterXMM0 _ = error "Type mismatch"
writeRegisterXMM RegisterXMM1 (ValueFloat f) =
  State.modify $ \env -> env {regXMM1 = f}
writeRegisterXMM RegisterXMM1 _ = error "Type mismatch"
writeRegisterXMM RegisterXMM2 (ValueFloat f) =
  State.modify $ \env -> env {regXMM2 = f}
writeRegisterXMM RegisterXMM2 _ = error "Type mismatch"
writeRegisterXMM RegisterXMM3 (ValueFloat f) =
  State.modify $ \env -> env {regXMM3 = f}
writeRegisterXMM RegisterXMM3 _ = error "Type mismatch"
writeRegisterXMM RegisterXMM4 (ValueFloat f) =
  State.modify $ \env -> env {regXMM4 = f}
writeRegisterXMM RegisterXMM4 _ = error "Type mismatch"
writeRegisterXMM RegisterXMM5 (ValueFloat f) =
  State.modify $ \env -> env {regXMM5 = f}
writeRegisterXMM RegisterXMM5 _ = error "Type mismatch"
writeRegisterXMM RegisterXMM6 (ValueFloat f) =
  State.modify $ \env -> env {regXMM6 = f}
writeRegisterXMM RegisterXMM6 _ = error "Type mismatch"
writeRegisterXMM RegisterXMM7 (ValueFloat f) =
  State.modify $ \env -> env {regXMM7 = f}
writeRegisterXMM RegisterXMM7 _ = error "Type mismatch"

readPointer :: Pointer -> Execute Value
readPointer Pointer {pointerBase = r, pointerDisplacement = d} = do
  ValueInt b <- readRegister r
  readFromStack (b + d)

writePointer :: Pointer -> Value -> Execute ()
writePointer Pointer {pointerBase = r, pointerDisplacement = d} val = do
  ValueInt b <- readRegister r
  writeToStack (b + d) val

readIntOperand :: IntOperand -> Execute Value
readIntOperand (IntOperandRegister _ r) = readRegister r
readIntOperand (IntOperandPointer p) = readPointer p

writeIntOperand :: IntOperand -> Value -> Execute ()
writeIntOperand (IntOperandRegister _ r) val = writeRegister r val
writeIntOperand (IntOperandPointer p) val = writePointer p val

jump :: LabelID -> Execute ()
jump (LabelID lid) = do
  ip <- Reader.asks ((IntMap.! lid) . constEnvLabelMap)
  State.modify $ \env -> env {regRIP = ip - 1}

execute :: Instruction -> Execute ()
execute (InstructionCALL fcall) = functionCall fcall
execute (InstructionCMP lhs rhs) = do
  ValueInt lhs' <- readRegister lhs
  ValueInt rhs' <- readIntOperand rhs
  let (zf, sf) =
        case compare lhs' rhs' of
          EQ -> (True, False)
          LT -> (False, True)
          GT -> (False, False)
  State.modify $ \env ->
    env {regEFLAGS = (regEFLAGS env) {efZF = zf, efSF = sf}}
execute (InstructionSetZ v) = do
  zf <- State.gets (efZF . regEFLAGS)
  writeRegister8
    v
    (if zf
       then ValueInt 1
       else ValueInt 0)
execute (InstructionSetNZ v) = do
  zf <- State.gets (efZF . regEFLAGS)
  writeRegister8
    v
    (if zf
       then ValueInt 0
       else ValueInt 1)
execute (InstructionSetS v) = do
  sf <- State.gets (efSF . regEFLAGS)
  writeRegister8
    v
    (if sf
       then ValueInt 1
       else ValueInt 0)
execute (InstructionSetC v) = do
  cf <- State.gets (efCF . regEFLAGS)
  writeRegister8
    v
    (if cf
       then ValueInt 1
       else ValueInt 0)
execute (InstructionMOV_R64_IMM64 lhs rhs) = do
  res <- readImmediate rhs
  writeRegister lhs res
execute (InstructionMOV_R64_RM64 lhs rhs) = do
  res <- readIntOperand rhs
  writeRegister lhs res
execute (InstructionMOV_RM64_R64 lhs rhs) = do
  res <- readRegister rhs
  writeIntOperand lhs res
execute InstructionRET = do
  ValueInt ip <- popFromStack VarTypeInt -- popping RIP
  State.modify $ \env -> env {regRIP = ip}
execute (InstructionLabelledNOP _) = pure ()
execute (InstructionJMP l) = jump l
execute (InstructionJZ l) = do
  zf <- State.gets (efZF . regEFLAGS)
  when zf $ jump l
execute (InstructionNEG v) = do
  ValueInt val <- readIntOperand v
  writeIntOperand v (ValueInt (negate val))
execute (InstructionAND lhs rhs) = do
  lhs' <- readRegister lhs
  rhs' <- readIntOperand rhs
  writeRegister lhs (lhs' .&. rhs')
execute (InstructionXOR lhs rhs) = do
  lhs' <- readRegister lhs
  rhs' <- readIntOperand rhs
  writeRegister lhs (lhs' `xor` rhs')
execute (InstructionOR lhs rhs) = do
  lhs' <- readRegister lhs
  rhs' <- readIntOperand rhs
  writeRegister lhs (lhs' .|. rhs')
execute (InstructionADD lhs rhs) = do
  lhs' <- readRegister lhs
  rhs' <- readIntOperand rhs
  writeRegister lhs (lhs' + rhs')
execute (InstructionSUB lhs rhs) = do
  lhs' <- readRegister lhs
  rhs' <- readIntOperand rhs
  writeRegister lhs (lhs' - rhs')
execute (InstructionIDIV v)
  -- TODO: Should use RDX:RAX
 = do
  lhs' <- readRegister RegisterRAX
  rhs' <- readIntOperand v
  let (q, r) = lhs' `quotRem` rhs'
  writeRegister RegisterRAX q
  writeRegister RegisterRDX r
execute (InstructionIMUL lhs rhs) = do
  lhs' <- readRegister lhs
  rhs' <- readIntOperand rhs
  writeRegister lhs (lhs' * rhs')
execute InstructionCQO
  -- TODO: Not implemented. We only use RAX.
 = pure ()
execute (InstructionADDSD lhs rhs) = do
  lhs' <- readRegisterXMM lhs
  rhs' <- readRegisterXMM rhs
  writeRegisterXMM lhs (lhs' + rhs')
execute (InstructionSUBSD lhs rhs) = do
  lhs' <- readRegisterXMM lhs
  rhs' <- readRegisterXMM rhs
  writeRegisterXMM lhs (lhs' - rhs')
execute (InstructionMULSD lhs rhs) = do
  lhs' <- readRegisterXMM lhs
  rhs' <- readRegisterXMM rhs
  writeRegisterXMM lhs (lhs' * rhs')
execute (InstructionDIVSD lhs rhs) = do
  lhs' <- readRegisterXMM lhs
  rhs' <- readRegisterXMM rhs
  writeRegisterXMM lhs (lhs' / rhs')
execute (InstructionCOMISD lhs rhs) = do
  lhs' <- readRegisterXMM lhs
  rhs' <- readRegisterXMM rhs
  let (zf, cf) =
        case compare lhs' rhs' of
          EQ -> (True, False)
          LT -> (False, True)
          GT -> (False, False)
  State.modify $ \env ->
    env {regEFLAGS = (regEFLAGS env) {efZF = zf, efCF = cf}}
execute (InstructionMOVSD_XMM_XMM lhs rhs) = do
  res <- readRegisterXMM rhs
  writeRegisterXMM lhs res
execute (InstructionMOVSD_XMM_M64 lhs rhs) = do
  res <- readPointer rhs
  writeRegisterXMM lhs res
execute (InstructionMOVSD_M64_XMM lhs rhs) = do
  res <- readRegisterXMM rhs
  writePointer lhs res
execute (InstructionCVTSI2SD lhs rhs) = do
  ValueInt i <- readIntOperand rhs
  writeRegisterXMM lhs $ ValueFloat $ fromIntegral i
execute (InstructionPUSH x) = do
  res <- readRegister x
  pushOnStack res
execute (InstructionPOP x) = do
  v <- popFromStack (intOperandType x)
  writeIntOperand x v
execute (InstructionLEA r (StringID sid)) = do
  s <- Reader.asks ((IntMap.! sid) . constEnvStrings)
  writeRegister r $ ValueString $ Right s
