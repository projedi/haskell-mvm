{-# LANGUAGE FlexibleContexts #-}

module ASM
  ( avenge
  ) where

import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Reader as Reader
import Control.Monad.State (MonadState, State, runState)
import qualified Control.Monad.State as State
import Control.Monad.Writer (MonadWriter, WriterT, execWriterT)
import qualified Control.Monad.Writer as Writer
import Data.Int (Int64)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Word (Word64)

import qualified ASMSyntax
import CallingConvention (CallingConvention)
import qualified CallingConvention
import qualified LinearSyntax

avenge :: LinearSyntax.Program -> ASMSyntax.Program
avenge p =
  ASMSyntax.Program
    { ASMSyntax.programCode = code
    , ASMSyntax.programLibraries = LinearSyntax.programLibraries p
    , ASMSyntax.programForeignFunctions = foreignFunctions finalEnv
    , ASMSyntax.programStrings = LinearSyntax.programStrings p
    , ASMSyntax.programLastFunID = LinearSyntax.programLastFunID p
    , ASMSyntax.programLastStringID = LinearSyntax.programLastStringID p
    , ASMSyntax.programLastLabelID = lastLabelID finalEnv
    }
  where
    (code, finalEnv) =
      runState (execWriterT (translateCode (LinearSyntax.programFunctions p))) $
      Env
        { varMap = IntMap.empty
        , currentStackSize = 0
        , lastLabelID = LinearSyntax.programLastLabelID p
        , funIdToLabelID = IntMap.empty
        , foreignFunctions = LinearSyntax.programForeignFunctions p
        }

data Var = Var
  { varType :: ASMSyntax.VarType
  , varDisplacement :: Int64 -- Displacement from RBP.
  }

data Env = Env
  { varMap :: IntMap Var
  , currentStackSize :: Int64
  , lastLabelID :: ASMSyntax.LabelID
  , funIdToLabelID :: IntMap ASMSyntax.LabelID
  , foreignFunctions :: IntMap ASMSyntax.ForeignFunctionDecl
  }

type ASM = WriterT [ASMSyntax.Instruction] (State Env)

nextLabel :: MonadState Env m => m ASMSyntax.LabelID
nextLabel = do
  lbl <- State.gets (inc . lastLabelID)
  State.modify $ \env -> env {lastLabelID = lbl}
  pure lbl
  where
    inc (ASMSyntax.LabelID lid) = ASMSyntax.LabelID (lid + 1)

opRBP :: ASMSyntax.IntOperand
opRBP = ASMSyntax.IntOperandRegister ASMSyntax.VarTypeInt ASMSyntax.RegisterRBP

opRSP :: ASMSyntax.IntOperand
opRSP = ASMSyntax.IntOperandRegister ASMSyntax.VarTypeInt ASMSyntax.RegisterRSP

opRAX :: ASMSyntax.VarType -> ASMSyntax.IntOperand
opRAX t = ASMSyntax.IntOperandRegister t ASMSyntax.RegisterRAX

opRBX :: ASMSyntax.VarType -> ASMSyntax.IntOperand
opRBX t = ASMSyntax.IntOperandRegister t ASMSyntax.RegisterRBX

opRCX :: ASMSyntax.VarType -> ASMSyntax.IntOperand
opRCX t = ASMSyntax.IntOperandRegister t ASMSyntax.RegisterRCX

opRDX :: ASMSyntax.VarType -> ASMSyntax.IntOperand
opRDX t = ASMSyntax.IntOperandRegister t ASMSyntax.RegisterRDX

introduceVariable :: LinearSyntax.Var -> ASM Var
introduceVariable LinearSyntax.Var { LinearSyntax.varName = LinearSyntax.VarID vid
                                   , LinearSyntax.varType = t
                                   } = do
  d <- State.gets currentStackSize
  let var = Var {varType = t, varDisplacement = d}
  State.modify $ \env ->
    env
      { varMap = IntMap.insert vid var $ varMap env
      , currentStackSize = ASMSyntax.typeSize t + currentStackSize env
      }
  pure var

resolveVariable :: MonadState Env m => LinearSyntax.Var -> m Var
resolveVariable LinearSyntax.Var {LinearSyntax.varName = LinearSyntax.VarID vid} =
  State.gets ((IntMap.! vid) . varMap)

resolveVariableAsPointer ::
     MonadState Env m => LinearSyntax.Var -> m ASMSyntax.Pointer
resolveVariableAsPointer v = do
  Var {varType = t, varDisplacement = d} <- resolveVariable v
  pure $
    ASMSyntax.Pointer
      { ASMSyntax.pointerType = t
      , ASMSyntax.pointerBase = ASMSyntax.RegisterRBP
      , ASMSyntax.pointerDisplacement =
          -d - ASMSyntax.typeSize (ASMSyntax.VarTypeInt)
      }

resolveVariableAsIntOperand ::
     MonadState Env m => LinearSyntax.Var -> m ASMSyntax.IntOperand
resolveVariableAsIntOperand v =
  ASMSyntax.IntOperandPointer <$> resolveVariableAsPointer v

translateCode :: IntMap LinearSyntax.FunctionDef -> ASM ()
translateCode fs = do
  let fids = IntMap.keys fs
  let generateLabelForFunID fid = do
        l <- nextLabel
        pure (fid, l)
  fidLabelMap <- mapM generateLabelForFunID fids
  let funMap = IntMap.fromList fidLabelMap
  State.modify $ \env -> env {funIdToLabelID = funMap}
  addStatement $ ASMSyntax.InstructionPUSH ASMSyntax.RegisterRSP
  State.modify $ \env ->
    env
      { currentStackSize =
          ASMSyntax.typeSize ASMSyntax.VarTypeInt + currentStackSize env
      }
  -- Make sure our stack is 16-byte aligned
  addStatement $
    ASMSyntax.InstructionMOV_R64_IMM64
      ASMSyntax.RegisterRAX
      (ASMSyntax.ImmediateInt $ fromIntegral (0xfffffffffffffff0 :: Word64))
  addStatement $
    ASMSyntax.InstructionAND ASMSyntax.RegisterRSP (opRAX ASMSyntax.VarTypeInt)
  addStatement $ ASMSyntax.InstructionCALL_DISP (funMap IntMap.! 0)
  addStatement $ ASMSyntax.InstructionPOP ASMSyntax.RegisterRSP
  addStatement ASMSyntax.InstructionRET
  mapM_ translateFunctionDef fs

retValueFromCallingConvention :: CallingConvention -> Maybe SomeRegister
retValueFromCallingConvention cc =
  case CallingConvention.funRetValue cc of
    Nothing -> Nothing
    Just (CallingConvention.RetLocationRegister t r) -> Just $ Left (t, r)
    Just (CallingConvention.RetLocationRegisterXMM r) -> Just $ Right r

translateFunctionDef :: LinearSyntax.FunctionDef -> ASM ()
translateFunctionDef fdef = do
  State.modify $ \env -> env {currentStackSize = 0}
  params <- mapM introduceVariable $ LinearSyntax.funDefParams fdef
  locals <- mapM introduceVariable $ LinearSyntax.funDefLocals fdef
  let cc =
        CallingConvention.computeCallingConvention
          CallingConvention.FunctionCall
            { CallingConvention.funRetType = LinearSyntax.funDefRetType fdef
            , CallingConvention.funArgTypes = map varType params
            }
  let retValue = retValueFromCallingConvention cc
  epilogueLbl <- nextLabel
  runASMStatement
    ConstEnv {retValueLocation = retValue, epilogueLabel = epilogueLbl} $ do
    let (LinearSyntax.FunID fid) = LinearSyntax.funDefName fdef
    funLbl <- State.gets ((IntMap.! fid) . funIdToLabelID)
    addStatement $ ASMSyntax.InstructionLabelledNOP funLbl
    let stackVars = params ++ locals
    addStatement $ ASMSyntax.InstructionPUSH ASMSyntax.RegisterRBP
    State.modify $ \env ->
      env
        { currentStackSize =
            ASMSyntax.typeSize ASMSyntax.VarTypeInt + currentStackSize env
        }
    addStatement $ ASMSyntax.InstructionMOV_R64_RM64 ASMSyntax.RegisterRBP opRSP
    let size = ASMSyntax.typesSize $ map varType stackVars
    addStatement $
      ASMSyntax.InstructionMOV_R64_IMM64
        ASMSyntax.RegisterRBX
        (ASMSyntax.ImmediateInt size)
    -- No need to account for these, as they have been accounted for in introduceVariable.
    addStatement $
      ASMSyntax.InstructionSUB
        ASMSyntax.RegisterRSP
        (opRBX ASMSyntax.VarTypeInt)
    prepareArgsAtCall params cc
    mapM_ translateStatement $ LinearSyntax.funDefBody fdef
    addStatement $ ASMSyntax.InstructionLabelledNOP epilogueLbl
    addStatement $ ASMSyntax.InstructionMOV_R64_RM64 ASMSyntax.RegisterRSP opRBP
    State.modify $ \env -> env {currentStackSize = currentStackSize env - size}
    addStatement $ ASMSyntax.InstructionPOP ASMSyntax.RegisterRBP
    addStatement ASMSyntax.InstructionRET

type ASMStatement = ReaderT ConstEnv ASM

data ConstEnv = ConstEnv
  { retValueLocation :: Maybe SomeRegister
  , epilogueLabel :: ASMSyntax.LabelID
  }

runASMStatement :: ConstEnv -> ASMStatement a -> ASM a
runASMStatement env m = runReaderT m env

addStatement ::
     MonadWriter [ASMSyntax.Instruction] m => ASMSyntax.Instruction -> m ()
addStatement s = Writer.tell [s]

translateStatement :: LinearSyntax.Statement -> ASMStatement ()
translateStatement (LinearSyntax.StatementFunctionCall fcall) = do
  _ <- translateFunctionCall fcall
  pure ()
translateStatement (LinearSyntax.StatementAssign v (LinearSyntax.ExprFunctionCall fcall)) = do
  cc <- translateFunctionCall fcall
  let (Just retValue) = retValueFromCallingConvention cc
  case retValue of
    Left (_, r) -> do
      v' <- resolveVariableAsIntOperand v
      addStatement $ ASMSyntax.InstructionMOV_RM64_R64 v' r
    Right r -> do
      v' <- resolveVariableAsPointer v
      addStatement $ ASMSyntax.InstructionMOVSD_M64_XMM v' r
translateStatement (LinearSyntax.StatementAssign v e) = do
  res <- translateExpr e
  case res of
    Left (_, r) -> do
      v' <- resolveVariableAsIntOperand v
      addStatement $ ASMSyntax.InstructionMOV_RM64_R64 v' r
    Right r -> do
      v' <- resolveVariableAsPointer v
      addStatement $ ASMSyntax.InstructionMOVSD_M64_XMM v' r
translateStatement (LinearSyntax.StatementAssignToPtr p v) = do
  p' <- resolveVariableAsIntOperand p
  addStatement $ ASMSyntax.InstructionMOV_R64_RM64 ASMSyntax.RegisterRAX p'
  v' <- resolveVariableAsIntOperand v
  addStatement $ ASMSyntax.InstructionMOV_R64_RM64 ASMSyntax.RegisterRCX v'
  addStatement $
    ASMSyntax.InstructionMOV_RM64_R64
      (ASMSyntax.IntOperandPointer
         ASMSyntax.Pointer
           { ASMSyntax.pointerType = ASMSyntax.intOperandType v'
           , ASMSyntax.pointerBase = ASMSyntax.RegisterRAX
           , ASMSyntax.pointerDisplacement = 0
           })
      ASMSyntax.RegisterRCX
translateStatement (LinearSyntax.StatementReturn Nothing) = do
  Nothing <- Reader.asks retValueLocation
  lbl <- Reader.asks epilogueLabel
  addStatement $ ASMSyntax.InstructionJMP lbl
translateStatement (LinearSyntax.StatementReturn (Just v)) = do
  Just rl <- Reader.asks retValueLocation
  case rl of
    Left (_, r) -> do
      v' <- resolveVariableAsIntOperand v
      addStatement $ ASMSyntax.InstructionMOV_R64_RM64 r v'
    Right r -> do
      v' <- resolveVariableAsPointer v
      addStatement $ ASMSyntax.InstructionMOVSD_XMM_M64 r v'
  lbl <- Reader.asks epilogueLabel
  addStatement $ ASMSyntax.InstructionJMP lbl
translateStatement (LinearSyntax.StatementLabel l) =
  addStatement $ ASMSyntax.InstructionLabelledNOP l
translateStatement (LinearSyntax.StatementJump l) =
  addStatement $ ASMSyntax.InstructionJMP l
translateStatement (LinearSyntax.StatementJumpIfZero v l) = do
  v' <- resolveVariableAsIntOperand v
  compareIntToZero v'
  addStatement $ ASMSyntax.InstructionJZ l

prepareArgsForCall ::
     CallingConvention -> [LinearSyntax.Var] -> Int64 -> ASMStatement ()
prepareArgsForCall cc args extraOffset = do
  let size =
        ASMSyntax.typesSize (CallingConvention.funStackToAllocate cc) +
        extraOffset
  addStatement $
    ASMSyntax.InstructionMOV_R64_IMM64
      ASMSyntax.RegisterRAX
      (ASMSyntax.ImmediateInt size)
  addStatement $
    ASMSyntax.InstructionSUB ASMSyntax.RegisterRSP (opRAX ASMSyntax.VarTypeInt)
  State.modify $ \env -> env {currentStackSize = currentStackSize env + size}
  mapM_ go (zip args (CallingConvention.funArgValues cc))
  addStatement $
    ASMSyntax.InstructionMOV_R64_IMM64
      ASMSyntax.RegisterRAX
      (ASMSyntax.ImmediateInt
         (fromIntegral $ CallingConvention.funFloatRegistersUsed cc))
  where
    go :: (LinearSyntax.Var, CallingConvention.ArgLocation) -> ASMStatement ()
    go (arg, CallingConvention.ArgLocationRegister _ r) = do
      arg' <- resolveVariableAsIntOperand arg
      addStatement $ ASMSyntax.InstructionMOV_R64_RM64 r arg'
    go (arg, CallingConvention.ArgLocationRegisterXMM r) = do
      arg' <- resolveVariableAsPointer arg
      addStatement $ ASMSyntax.InstructionMOVSD_XMM_M64 r arg'
    go (arg, CallingConvention.ArgLocationStack t d) = do
      arg' <- resolveVariableAsIntOperand arg
      addStatement $
        ASMSyntax.InstructionMOV_R64_RM64 ASMSyntax.RegisterRBX arg'
      addStatement $
        ASMSyntax.InstructionMOV_RM64_R64
          (ASMSyntax.IntOperandPointer $ pointerForStack t d)
          ASMSyntax.RegisterRBX
    pointerForStack :: ASMSyntax.VarType -> Int64 -> ASMSyntax.Pointer
    pointerForStack t d =
      ASMSyntax.Pointer
        { ASMSyntax.pointerType = t
        , ASMSyntax.pointerBase = ASMSyntax.RegisterRSP
        , ASMSyntax.pointerDisplacement = d
        }

cleanStackAfterCall :: CallingConvention -> Int64 -> ASMStatement ()
cleanStackAfterCall cc extraOffset = do
  let size =
        ASMSyntax.typesSize (CallingConvention.funStackToAllocate cc) +
        extraOffset
  addStatement $
    ASMSyntax.InstructionMOV_R64_IMM64
      ASMSyntax.RegisterRCX
      (ASMSyntax.ImmediateInt size)
  addStatement $
    ASMSyntax.InstructionADD ASMSyntax.RegisterRSP (opRCX ASMSyntax.VarTypeInt)
  State.modify $ \env -> env {currentStackSize = currentStackSize env - size}

prepareArgsAtCall :: [Var] -> CallingConvention -> ASMStatement ()
prepareArgsAtCall params cc = do
  let vals = map go (CallingConvention.funArgValues cc)
  generateAssignments params vals
  where
    go :: CallingConvention.ArgLocation -> Var -> [ASMSyntax.Instruction]
    go (CallingConvention.ArgLocationRegister _ r) v =
      [ ASMSyntax.InstructionMOV_RM64_R64
          (ASMSyntax.IntOperandPointer $ pointerForLocalVar v)
          r
      ]
    go (CallingConvention.ArgLocationRegisterXMM r) v =
      [ASMSyntax.InstructionMOVSD_M64_XMM (pointerForLocalVar v) r]
    go (CallingConvention.ArgLocationStack t d) v =
      [ ASMSyntax.InstructionMOV_R64_RM64
          ASMSyntax.RegisterRBX
          (ASMSyntax.IntOperandPointer $ pointerForParamVar t d)
      , ASMSyntax.InstructionMOV_RM64_R64
          (ASMSyntax.IntOperandPointer $ pointerForLocalVar v)
          ASMSyntax.RegisterRBX
      ]
    generateAssignments ::
         [Var] -> [Var -> [ASMSyntax.Instruction]] -> ASMStatement ()
    generateAssignments [] [] = pure ()
    generateAssignments (v:vs) (val:vals) = do
      mapM_ addStatement $ val v
      generateAssignments vs vals
    generateAssignments _ _ = error "Type mismatch"
    pointerForParamVar :: ASMSyntax.VarType -> Int64 -> ASMSyntax.Pointer
    pointerForParamVar t d =
      ASMSyntax.Pointer
        { ASMSyntax.pointerType = t
        , ASMSyntax.pointerBase = ASMSyntax.RegisterRBP
        , ASMSyntax.pointerDisplacement =
            d + 2 * ASMSyntax.typeSize ASMSyntax.VarTypeInt
        }
    pointerForLocalVar :: Var -> ASMSyntax.Pointer
    pointerForLocalVar Var {varType = t, varDisplacement = d} =
      ASMSyntax.Pointer
        { ASMSyntax.pointerType = t
        , ASMSyntax.pointerBase = ASMSyntax.RegisterRBP
        , ASMSyntax.pointerDisplacement =
            -d - ASMSyntax.typeSize ASMSyntax.VarTypeInt
        }

computeExtraOffsetForCall :: CallingConvention -> ASMStatement Int64
computeExtraOffsetForCall cc = do
  size <- State.gets currentStackSize
  let argsSize = ASMSyntax.typesSize (CallingConvention.funStackToAllocate cc)
  let ripSize = ASMSyntax.typeSize (ASMSyntax.VarTypeInt)
  pure $ 16 - ((size + argsSize + ripSize) `rem` 16)

translateFunctionCall ::
     LinearSyntax.FunctionCall -> ASMStatement CallingConvention
translateFunctionCall fcall@LinearSyntax.NativeFunctionCall {} = do
  let args = LinearSyntax.nativeFunCallArgs fcall
  let cc =
        CallingConvention.computeCallingConvention
          CallingConvention.FunctionCall
            { CallingConvention.funRetType =
                LinearSyntax.nativeFunCallRetType fcall
            , CallingConvention.funArgTypes = map LinearSyntax.varType args
            }
  extraOffset <- computeExtraOffsetForCall cc
  prepareArgsForCall cc args extraOffset
  let (LinearSyntax.FunID fid) = LinearSyntax.nativeFunCallName fcall
  flbl <- State.gets ((IntMap.! fid) . funIdToLabelID)
  addStatement $ ASMSyntax.InstructionCALL_DISP flbl
  cleanStackAfterCall cc extraOffset
  pure cc
translateFunctionCall fcall@LinearSyntax.ForeignFunctionCall {LinearSyntax.foreignFunCallName = ASMSyntax.FunID fid} = do
  let args = LinearSyntax.foreignFunCallArgs fcall
  let cc =
        CallingConvention.computeCallingConvention
          CallingConvention.FunctionCall
            { CallingConvention.funRetType =
                LinearSyntax.foreignFunCallRetType fcall
            , CallingConvention.funArgTypes = map LinearSyntax.varType args
            }
  extraOffset <- computeExtraOffsetForCall cc
  prepareArgsForCall cc args extraOffset
  fdecl <- State.gets ((IntMap.! fid) . foreignFunctions)
  addStatement $
    ASMSyntax.InstructionMOV_R64_FunID
      ASMSyntax.RegisterRBX
      (ASMSyntax.FunID fid)
  addStatement $
    ASMSyntax.InstructionCALL_RM64
      ASMSyntax.ForeignFunctionCall
        { ASMSyntax.foreignFunCallName = ASMSyntax.FunID fid
        , ASMSyntax.foreignFunCallRealName =
            ASMSyntax.foreignFunDeclRealName fdecl
        , ASMSyntax.foreignFunCallRetType =
            LinearSyntax.foreignFunCallRetType fcall
        , ASMSyntax.foreignFunCallArgTypes = map LinearSyntax.varType args
        }
      (opRBX ASMSyntax.VarTypeInt)
  cleanStackAfterCall cc extraOffset
  pure cc

type SomeRegister
   = Either (ASMSyntax.VarType, ASMSyntax.Register) ASMSyntax.RegisterXMM

translateExpr :: LinearSyntax.Expr -> ASMStatement SomeRegister
translateExpr (LinearSyntax.ExprFunctionCall _) =
  error "Must've been handled in translateStatement"
translateExpr (LinearSyntax.ExprVar v) = do
  v' <- resolveVariableAsIntOperand v
  addStatement $ ASMSyntax.InstructionMOV_R64_RM64 ASMSyntax.RegisterRAX v'
  pure $ Left (ASMSyntax.intOperandType v', ASMSyntax.RegisterRAX)
translateExpr (LinearSyntax.ExprDereference v) = do
  v' <- resolveVariableAsIntOperand v
  addStatement $ ASMSyntax.InstructionMOV_R64_RM64 ASMSyntax.RegisterRCX v'
  addStatement $
    ASMSyntax.InstructionMOV_R64_RM64
      ASMSyntax.RegisterRAX
      (ASMSyntax.IntOperandPointer
         ASMSyntax.Pointer
           { ASMSyntax.pointerBase = ASMSyntax.RegisterRCX
           , ASMSyntax.pointerDisplacement = 0
           , ASMSyntax.pointerType = ASMSyntax.intOperandType v'
           })
  pure $ Left (ASMSyntax.intOperandType v', ASMSyntax.RegisterRAX)
translateExpr (LinearSyntax.ExprAddressOf v) = do
  Var {varDisplacement = d} <- resolveVariable v
  addStatement $
    ASMSyntax.InstructionMOV_R64_IMM64
      ASMSyntax.RegisterRCX
      (ASMSyntax.ImmediateInt (d + ASMSyntax.typeSize ASMSyntax.VarTypeInt))
  addStatement $ ASMSyntax.InstructionMOV_R64_RM64 ASMSyntax.RegisterRAX opRBP
  addStatement $
    ASMSyntax.InstructionSUB ASMSyntax.RegisterRAX (opRCX ASMSyntax.VarTypeInt)
  pure $ Left (ASMSyntax.VarTypeInt, ASMSyntax.RegisterRAX)
translateExpr (LinearSyntax.ExprConst c) = do
  case c of
    LinearSyntax.ImmediateInt i ->
      addStatement $
      ASMSyntax.InstructionMOV_R64_IMM64
        ASMSyntax.RegisterRAX
        (ASMSyntax.ImmediateInt i)
    LinearSyntax.ImmediateFloat f ->
      addStatement $
      ASMSyntax.InstructionMOV_R64_IMM64
        ASMSyntax.RegisterRAX
        (ASMSyntax.ImmediateFloat f)
    LinearSyntax.ImmediateString s ->
      addStatement $ ASMSyntax.InstructionLEA ASMSyntax.RegisterRAX s
  pure $ Left (LinearSyntax.immediateType c, ASMSyntax.RegisterRAX)
translateExpr (LinearSyntax.ExprBinOp op lhs rhs) = translateBinOp op lhs rhs
translateExpr (LinearSyntax.ExprUnOp op v) = translateUnOp op v

translateUnOp ::
     LinearSyntax.UnOp -> LinearSyntax.Var -> ASMStatement SomeRegister
translateUnOp LinearSyntax.UnNeg v =
  case LinearSyntax.varType v of
    ASMSyntax.VarTypeInt -> do
      v' <- resolveVariableAsIntOperand v
      let res = opRAX ASMSyntax.VarTypeInt
      addStatement $ ASMSyntax.InstructionMOV_R64_RM64 ASMSyntax.RegisterRAX v'
      addStatement $ ASMSyntax.InstructionNEG res
      pure $ Left (ASMSyntax.VarTypeInt, ASMSyntax.RegisterRAX)
    ASMSyntax.VarTypeFloat -> do
      let rax = opRAX ASMSyntax.VarTypeInt
      let xmm0 = ASMSyntax.RegisterXMM0
      addStatement $
        ASMSyntax.InstructionMOV_R64_IMM64
          ASMSyntax.RegisterRAX
          (ASMSyntax.ImmediateInt 0)
      addStatement $ ASMSyntax.InstructionCVTSI2SD xmm0 rax
      v' <- resolveVariableAsPointer v
      let xmm1 = ASMSyntax.RegisterXMM1
      addStatement $ ASMSyntax.InstructionMOVSD_XMM_M64 xmm1 v'
      addStatement $ ASMSyntax.InstructionSUBSD xmm0 xmm1
      pure $ Right xmm0
    _ -> error "Type mismatch"
translateUnOp LinearSyntax.UnNot v = do
  v' <- resolveVariableAsIntOperand v
  compareIntToZero v'
  addStatement $
    ASMSyntax.InstructionMOV_R64_IMM64
      ASMSyntax.RegisterRAX
      (ASMSyntax.ImmediateInt 0)
  addStatement $ ASMSyntax.InstructionSetZ ASMSyntax.RegisterAL
  pure $ Left (ASMSyntax.VarTypeInt, ASMSyntax.RegisterRAX)
translateUnOp LinearSyntax.UnIntToFloat v = do
  v' <- resolveVariableAsIntOperand v
  let res = ASMSyntax.RegisterXMM0
  addStatement $ ASMSyntax.InstructionCVTSI2SD res v'
  pure $ Right res

translateBinOp ::
     LinearSyntax.BinOp
  -> LinearSyntax.Var
  -> LinearSyntax.Var
  -> ASMStatement SomeRegister
translateBinOp LinearSyntax.BinPlus lhs rhs =
  case (LinearSyntax.varType lhs, LinearSyntax.varType rhs) of
    (ASMSyntax.VarTypeInt, ASMSyntax.VarTypeInt) -> do
      lhs' <- resolveVariableAsIntOperand lhs
      rhs' <- resolveVariableAsIntOperand rhs
      addStatement $
        ASMSyntax.InstructionMOV_R64_RM64 ASMSyntax.RegisterRAX lhs'
      addStatement $ ASMSyntax.InstructionADD ASMSyntax.RegisterRAX rhs'
      pure $ Left (ASMSyntax.VarTypeInt, ASMSyntax.RegisterRAX)
    (ASMSyntax.VarTypeFloat, ASMSyntax.VarTypeFloat) -> do
      lhs' <- resolveVariableAsPointer lhs
      rhs' <- resolveVariableAsPointer rhs
      let xmm0 = ASMSyntax.RegisterXMM0
      let xmm1 = ASMSyntax.RegisterXMM1
      addStatement $ ASMSyntax.InstructionMOVSD_XMM_M64 xmm0 lhs'
      addStatement $ ASMSyntax.InstructionMOVSD_XMM_M64 xmm1 rhs'
      addStatement $ ASMSyntax.InstructionADDSD xmm0 xmm1
      pure $ Right xmm0
    _ -> error "Type mismatch"
translateBinOp LinearSyntax.BinMinus lhs rhs =
  case (LinearSyntax.varType lhs, LinearSyntax.varType rhs) of
    (ASMSyntax.VarTypeInt, ASMSyntax.VarTypeInt) -> do
      lhs' <- resolveVariableAsIntOperand lhs
      rhs' <- resolveVariableAsIntOperand rhs
      addStatement $
        ASMSyntax.InstructionMOV_R64_RM64 ASMSyntax.RegisterRAX lhs'
      addStatement $ ASMSyntax.InstructionSUB ASMSyntax.RegisterRAX rhs'
      pure $ Left (ASMSyntax.VarTypeInt, ASMSyntax.RegisterRAX)
    (ASMSyntax.VarTypeFloat, ASMSyntax.VarTypeFloat) -> do
      lhs' <- resolveVariableAsPointer lhs
      rhs' <- resolveVariableAsPointer rhs
      let xmm0 = ASMSyntax.RegisterXMM0
      let xmm1 = ASMSyntax.RegisterXMM1
      addStatement $ ASMSyntax.InstructionMOVSD_XMM_M64 xmm0 lhs'
      addStatement $ ASMSyntax.InstructionMOVSD_XMM_M64 xmm1 rhs'
      addStatement $ ASMSyntax.InstructionSUBSD xmm0 xmm1
      pure $ Right xmm0
    _ -> error "Type mismatch"
translateBinOp LinearSyntax.BinTimes lhs rhs =
  case (LinearSyntax.varType lhs, LinearSyntax.varType rhs) of
    (ASMSyntax.VarTypeInt, ASMSyntax.VarTypeInt) -> do
      lhs' <- resolveVariableAsIntOperand lhs
      rhs' <- resolveVariableAsIntOperand rhs
      addStatement $
        ASMSyntax.InstructionMOV_R64_RM64 ASMSyntax.RegisterRAX lhs'
      addStatement $ ASMSyntax.InstructionIMUL ASMSyntax.RegisterRAX rhs'
      pure $ Left (ASMSyntax.VarTypeInt, ASMSyntax.RegisterRAX)
    (ASMSyntax.VarTypeFloat, ASMSyntax.VarTypeFloat) -> do
      lhs' <- resolveVariableAsPointer lhs
      rhs' <- resolveVariableAsPointer rhs
      let xmm0 = ASMSyntax.RegisterXMM0
      let xmm1 = ASMSyntax.RegisterXMM1
      addStatement $ ASMSyntax.InstructionMOVSD_XMM_M64 xmm0 lhs'
      addStatement $ ASMSyntax.InstructionMOVSD_XMM_M64 xmm1 rhs'
      addStatement $ ASMSyntax.InstructionMULSD xmm0 xmm1
      pure $ Right xmm0
    _ -> error "Type mismatch"
translateBinOp LinearSyntax.BinDiv lhs rhs =
  case (LinearSyntax.varType lhs, LinearSyntax.varType rhs) of
    (ASMSyntax.VarTypeInt, ASMSyntax.VarTypeInt) -> do
      lhs' <- resolveVariableAsIntOperand lhs
      rhs' <- resolveVariableAsIntOperand rhs
      addStatement $
        ASMSyntax.InstructionMOV_R64_RM64 ASMSyntax.RegisterRAX lhs'
      addStatement ASMSyntax.InstructionCQO
      addStatement $ ASMSyntax.InstructionIDIV rhs'
      pure $ Left (ASMSyntax.VarTypeInt, ASMSyntax.RegisterRAX)
    (ASMSyntax.VarTypeFloat, ASMSyntax.VarTypeFloat) -> do
      lhs' <- resolveVariableAsPointer lhs
      rhs' <- resolveVariableAsPointer rhs
      let xmm0 = ASMSyntax.RegisterXMM0
      let xmm1 = ASMSyntax.RegisterXMM1
      addStatement $ ASMSyntax.InstructionMOVSD_XMM_M64 xmm0 lhs'
      addStatement $ ASMSyntax.InstructionMOVSD_XMM_M64 xmm1 rhs'
      addStatement $ ASMSyntax.InstructionDIVSD xmm0 xmm1
      pure $ Right xmm0
    _ -> error "Type mismatch"
translateBinOp LinearSyntax.BinMod lhs rhs =
  case (LinearSyntax.varType lhs, LinearSyntax.varType rhs) of
    (ASMSyntax.VarTypeInt, ASMSyntax.VarTypeInt) -> do
      lhs' <- resolveVariableAsIntOperand lhs
      rhs' <- resolveVariableAsIntOperand rhs
      addStatement $
        ASMSyntax.InstructionMOV_R64_RM64 ASMSyntax.RegisterRAX lhs'
      addStatement ASMSyntax.InstructionCQO
      addStatement $ ASMSyntax.InstructionIDIV rhs'
      addStatement $
        ASMSyntax.InstructionMOV_R64_RM64
          ASMSyntax.RegisterRAX
          (opRDX ASMSyntax.VarTypeInt)
      pure $ Left (ASMSyntax.VarTypeInt, ASMSyntax.RegisterRAX)
    _ -> error "Type mismatch"
translateBinOp LinearSyntax.BinBitAnd lhs rhs = do
  lhs' <- resolveVariableAsIntOperand lhs
  rhs' <- resolveVariableAsIntOperand rhs
  addStatement $ ASMSyntax.InstructionMOV_R64_RM64 ASMSyntax.RegisterRAX lhs'
  addStatement $ ASMSyntax.InstructionAND ASMSyntax.RegisterRAX rhs'
  pure $ Left (ASMSyntax.VarTypeInt, ASMSyntax.RegisterRAX)
translateBinOp LinearSyntax.BinBitOr lhs rhs = do
  lhs' <- resolveVariableAsIntOperand lhs
  rhs' <- resolveVariableAsIntOperand rhs
  addStatement $ ASMSyntax.InstructionMOV_R64_RM64 ASMSyntax.RegisterRAX lhs'
  addStatement $ ASMSyntax.InstructionOR ASMSyntax.RegisterRAX rhs'
  pure $ Left (ASMSyntax.VarTypeInt, ASMSyntax.RegisterRAX)
translateBinOp LinearSyntax.BinBitXor lhs rhs = do
  lhs' <- resolveVariableAsIntOperand lhs
  rhs' <- resolveVariableAsIntOperand rhs
  addStatement $ ASMSyntax.InstructionMOV_R64_RM64 ASMSyntax.RegisterRAX lhs'
  addStatement $ ASMSyntax.InstructionXOR ASMSyntax.RegisterRAX rhs'
  pure $ Left (ASMSyntax.VarTypeInt, ASMSyntax.RegisterRAX)
translateBinOp LinearSyntax.BinAnd lhs rhs = do
  lhs' <- resolveVariableAsIntOperand lhs
  rhs' <- resolveVariableAsIntOperand rhs
  compareIntToZero lhs'
  let lhs'' = opRCX ASMSyntax.VarTypeInt
  addStatement $
    ASMSyntax.InstructionMOV_R64_IMM64
      ASMSyntax.RegisterRCX
      (ASMSyntax.ImmediateInt 0)
  addStatement $ ASMSyntax.InstructionSetNZ ASMSyntax.RegisterCL
  compareIntToZero rhs'
  let rhs'' = opRDX ASMSyntax.VarTypeInt
  addStatement $
    ASMSyntax.InstructionMOV_R64_IMM64
      ASMSyntax.RegisterRDX
      (ASMSyntax.ImmediateInt 0)
  addStatement $ ASMSyntax.InstructionSetNZ ASMSyntax.RegisterDL
  addStatement $ ASMSyntax.InstructionMOV_R64_RM64 ASMSyntax.RegisterRAX lhs''
  addStatement $ ASMSyntax.InstructionAND ASMSyntax.RegisterRAX rhs''
  pure $ Left (ASMSyntax.VarTypeInt, ASMSyntax.RegisterRAX)
translateBinOp LinearSyntax.BinOr lhs rhs = do
  lhs' <- resolveVariableAsIntOperand lhs
  rhs' <- resolveVariableAsIntOperand rhs
  compareIntToZero lhs'
  let lhs'' = opRCX ASMSyntax.VarTypeInt
  addStatement $
    ASMSyntax.InstructionMOV_R64_IMM64
      ASMSyntax.RegisterRCX
      (ASMSyntax.ImmediateInt 0)
  addStatement $ ASMSyntax.InstructionSetNZ ASMSyntax.RegisterCL
  compareIntToZero rhs'
  let rhs'' = opRDX ASMSyntax.VarTypeInt
  addStatement $
    ASMSyntax.InstructionMOV_R64_IMM64
      ASMSyntax.RegisterRDX
      (ASMSyntax.ImmediateInt 0)
  addStatement $ ASMSyntax.InstructionSetNZ ASMSyntax.RegisterDL
  addStatement $ ASMSyntax.InstructionMOV_R64_RM64 ASMSyntax.RegisterRAX lhs''
  addStatement $ ASMSyntax.InstructionOR ASMSyntax.RegisterRAX rhs''
  pure $ Left (ASMSyntax.VarTypeInt, ASMSyntax.RegisterRAX)
translateBinOp LinearSyntax.BinEq lhs rhs =
  case (LinearSyntax.varType lhs, LinearSyntax.varType rhs) of
    (ASMSyntax.VarTypeInt, ASMSyntax.VarTypeInt) -> do
      lhs' <- resolveVariableAsIntOperand lhs
      rhs' <- resolveVariableAsIntOperand rhs
      addStatement $
        ASMSyntax.InstructionMOV_R64_RM64 ASMSyntax.RegisterRAX lhs'
      addStatement $ ASMSyntax.InstructionCMP ASMSyntax.RegisterRAX rhs'
      addStatement $
        ASMSyntax.InstructionMOV_R64_IMM64
          ASMSyntax.RegisterRAX
          (ASMSyntax.ImmediateInt 0)
      addStatement $ ASMSyntax.InstructionSetZ ASMSyntax.RegisterAL
      pure $ Left (ASMSyntax.VarTypeInt, ASMSyntax.RegisterRAX)
    (ASMSyntax.VarTypeFloat, ASMSyntax.VarTypeFloat) -> do
      lhs' <- resolveVariableAsPointer lhs
      rhs' <- resolveVariableAsPointer rhs
      let xmm0 = ASMSyntax.RegisterXMM0
      let xmm1 = ASMSyntax.RegisterXMM1
      addStatement $ ASMSyntax.InstructionMOVSD_XMM_M64 xmm0 lhs'
      addStatement $ ASMSyntax.InstructionMOVSD_XMM_M64 xmm1 rhs'
      addStatement $ ASMSyntax.InstructionCOMISD xmm0 xmm1
      addStatement $
        ASMSyntax.InstructionMOV_R64_IMM64
          ASMSyntax.RegisterRAX
          (ASMSyntax.ImmediateInt 0)
      addStatement $ ASMSyntax.InstructionSetZ ASMSyntax.RegisterAL
      pure $ Left (ASMSyntax.VarTypeInt, ASMSyntax.RegisterRAX)
    _ -> error "Type mismatch"
translateBinOp LinearSyntax.BinLt lhs rhs =
  case (LinearSyntax.varType lhs, LinearSyntax.varType rhs) of
    (ASMSyntax.VarTypeInt, ASMSyntax.VarTypeInt) -> do
      lhs' <- resolveVariableAsIntOperand lhs
      rhs' <- resolveVariableAsIntOperand rhs
      addStatement $
        ASMSyntax.InstructionMOV_R64_RM64 ASMSyntax.RegisterRAX lhs'
      addStatement $ ASMSyntax.InstructionCMP ASMSyntax.RegisterRAX rhs'
      addStatement $
        ASMSyntax.InstructionMOV_R64_IMM64
          ASMSyntax.RegisterRAX
          (ASMSyntax.ImmediateInt 0)
      addStatement $ ASMSyntax.InstructionSetS ASMSyntax.RegisterAL
      pure $ Left (ASMSyntax.VarTypeInt, ASMSyntax.RegisterRAX)
    (ASMSyntax.VarTypeFloat, ASMSyntax.VarTypeFloat) -> do
      lhs' <- resolveVariableAsPointer lhs
      rhs' <- resolveVariableAsPointer rhs
      let xmm0 = ASMSyntax.RegisterXMM0
      let xmm1 = ASMSyntax.RegisterXMM1
      addStatement $ ASMSyntax.InstructionMOVSD_XMM_M64 xmm0 lhs'
      addStatement $ ASMSyntax.InstructionMOVSD_XMM_M64 xmm1 rhs'
      addStatement $ ASMSyntax.InstructionCOMISD xmm0 xmm1
      addStatement $
        ASMSyntax.InstructionMOV_R64_IMM64
          ASMSyntax.RegisterRAX
          (ASMSyntax.ImmediateInt 0)
      addStatement $ ASMSyntax.InstructionSetC ASMSyntax.RegisterAL
      pure $ Left (ASMSyntax.VarTypeInt, ASMSyntax.RegisterRAX)
    _ -> error "Type mismatch"

-- Compare int operand to zero and set EFLAGS.
compareIntToZero :: ASMSyntax.IntOperand -> ASMStatement ()
compareIntToZero v = do
  addStatement $
    ASMSyntax.InstructionMOV_R64_IMM64
      ASMSyntax.RegisterRAX
      (ASMSyntax.ImmediateInt 0)
  addStatement $ ASMSyntax.InstructionCMP ASMSyntax.RegisterRAX v
