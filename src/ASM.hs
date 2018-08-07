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

import qualified ASMSyntax
import CallingConvention (CallingConvention)
import qualified CallingConvention
import qualified LinearSyntax

avenge :: LinearSyntax.Program -> ASMSyntax.Program
avenge p =
  ASMSyntax.Program
    { ASMSyntax.programCode = ASMSyntax.FunctionDef code
    , ASMSyntax.programLibraries = LinearSyntax.programLibraries p
    , ASMSyntax.programForeignFunctions = LinearSyntax.programForeignFunctions p
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
        , varStack = []
        , lastLabelID = LinearSyntax.programLastLabelID p
        , funIdToLabelID = IntMap.empty
        }

data Var = Var
  { varType :: ASMSyntax.VarType
  , varDisplacement :: Int64 -- Displacement from RBP.
  }

data Env = Env
  { varMap :: IntMap Var
  , varStack :: [ASMSyntax.VarType]
  , lastLabelID :: ASMSyntax.LabelID
  , funIdToLabelID :: IntMap ASMSyntax.LabelID
  }

type ASM = WriterT [ASMSyntax.Statement] (State Env)

nextLabel :: MonadState Env m => m ASMSyntax.LabelID
nextLabel = do
  lbl <- State.gets (inc . lastLabelID)
  State.modify $ \env -> env {lastLabelID = lbl}
  pure lbl
  where
    inc (ASMSyntax.LabelID lid) = ASMSyntax.LabelID (lid + 1)

opRBP :: ASMSyntax.Operand
opRBP = ASMSyntax.OperandRegister ASMSyntax.VarTypeInt ASMSyntax.RegisterRBP

opRSP :: ASMSyntax.Operand
opRSP = ASMSyntax.OperandRegister ASMSyntax.VarTypeInt ASMSyntax.RegisterRSP

opRAX :: ASMSyntax.VarType -> ASMSyntax.Operand
opRAX t = ASMSyntax.OperandRegister t ASMSyntax.RegisterRAX

opRCX :: ASMSyntax.VarType -> ASMSyntax.Operand
opRCX t = ASMSyntax.OperandRegister t ASMSyntax.RegisterRCX

opRDX :: ASMSyntax.VarType -> ASMSyntax.Operand
opRDX t = ASMSyntax.OperandRegister t ASMSyntax.RegisterRDX

peekStack :: ASMSyntax.VarType -> ASMSyntax.Operand
peekStack t =
  ASMSyntax.OperandPointer
    ASMSyntax.Pointer
      { ASMSyntax.pointerType = t
      , ASMSyntax.pointerBase = Just ASMSyntax.RegisterRSP
      , ASMSyntax.pointerDisplacement = -1
      }

introduceVariable :: LinearSyntax.Var -> ASM Var
introduceVariable LinearSyntax.Var { LinearSyntax.varName = LinearSyntax.VarID vid
                                   , LinearSyntax.varType = t
                                   } = do
  d <- State.gets (fromIntegral . length . varStack)
  let var = Var {varType = t, varDisplacement = d}
  State.modify $ \env ->
    env
      {varMap = IntMap.insert vid var $ varMap env, varStack = t : varStack env}
  pure var

resolveVariable :: MonadState Env m => LinearSyntax.Var -> m Var
resolveVariable LinearSyntax.Var {LinearSyntax.varName = LinearSyntax.VarID vid} =
  State.gets ((IntMap.! vid) . varMap)

resolveVariableAsOperand ::
     MonadState Env m => LinearSyntax.Var -> m ASMSyntax.Operand
resolveVariableAsOperand v = do
  Var {varType = t, varDisplacement = d} <- resolveVariable v
  pure $
    ASMSyntax.OperandPointer
      ASMSyntax.Pointer
        { ASMSyntax.pointerType = t
        , ASMSyntax.pointerBase = Just ASMSyntax.RegisterRBP
        , ASMSyntax.pointerDisplacement = d
        }

translateCode :: IntMap LinearSyntax.FunctionDef -> ASM ()
translateCode fs = do
  let fids = IntMap.keys fs
  let generateLabelForFunID fid = do
        l <- nextLabel
        pure (fid, l)
  fidLabelMap <- mapM generateLabelForFunID fids
  let funMap = IntMap.fromList fidLabelMap
  State.modify $ \env -> env {funIdToLabelID = funMap}
  addStatement $
    ASMSyntax.InstructionCALL $
    ASMSyntax.NativeFunctionCall
      {ASMSyntax.nativeFunCallName = funMap IntMap.! 0}
  addStatement $ ASMSyntax.InstructionRET
  mapM_ translateFunctionDef fs

translateFunctionDef :: LinearSyntax.FunctionDef -> ASM ()
translateFunctionDef fdef = do
  State.modify $ \env -> env {varStack = []}
  params <- mapM introduceVariable $ LinearSyntax.funDefParams fdef
  locals <- mapM introduceVariable $ LinearSyntax.funDefLocals fdef
  State.modify $ \env -> env {varStack = []}
  let cc =
        CallingConvention.computeCallingConvention
          CallingConvention.FunctionCall
            { CallingConvention.funRetType = LinearSyntax.funDefRetType fdef
            , CallingConvention.funArgTypes = map varType params
            }
  let retValue =
        (uncurry ASMSyntax.OperandRegister) <$> CallingConvention.funRetValue cc
  epilogueLbl <- nextLabel
  runASMStatement
    ConstEnv {retValueLocation = retValue, epilogueLabel = epilogueLbl} $ do
    let (LinearSyntax.FunID fid) = LinearSyntax.funDefName fdef
    funLbl <- State.gets ((IntMap.! fid) . funIdToLabelID)
    addStatement $ ASMSyntax.InstructionLabelledNOP funLbl
    let stackVars = params ++ locals
    addStatement $ ASMSyntax.StatementPushOnStack opRBP
    addStatement $ ASMSyntax.InstructionMOV opRBP (Left opRSP)
    mapM_
      (addStatement . ASMSyntax.StatementAllocateOnStack . varType)
      stackVars
    prepareArgsAtCall params cc
    mapM_ translateStatement $ LinearSyntax.funDefBody fdef
    addStatement $ ASMSyntax.InstructionLabelledNOP epilogueLbl
    mapM_ (addStatement . ASMSyntax.StatementPopFromStack . varType) $
      reverse stackVars
    addStatement $
      ASMSyntax.InstructionMOV opRBP (Left $ peekStack ASMSyntax.VarTypeInt)
    addStatement $ ASMSyntax.StatementPopFromStack ASMSyntax.VarTypeInt
    addStatement $ ASMSyntax.InstructionRET

type ASMStatement = ReaderT ConstEnv ASM

data ConstEnv = ConstEnv
  { retValueLocation :: Maybe ASMSyntax.Operand
  , epilogueLabel :: ASMSyntax.LabelID
  }

runASMStatement :: ConstEnv -> ASMStatement a -> ASM a
runASMStatement env m = runReaderT m env

addStatement ::
     MonadWriter [ASMSyntax.Statement] m => ASMSyntax.Statement -> m ()
addStatement s = Writer.tell [s]

translateStatement :: LinearSyntax.Statement -> ASMStatement ()
translateStatement (LinearSyntax.StatementFunctionCall fcall) = do
  _ <- translateFunctionCall fcall
  pure ()
translateStatement (LinearSyntax.StatementAssign v (LinearSyntax.ExprFunctionCall fcall)) = do
  v' <- resolveVariableAsOperand v
  cc <- translateFunctionCall fcall
  let (Just retValue) =
        (uncurry ASMSyntax.OperandRegister) <$> CallingConvention.funRetValue cc
  addStatement $ ASMSyntax.InstructionMOV v' (Left retValue)
translateStatement (LinearSyntax.StatementAssign v e) = do
  translateExpr e
  v' <- resolveVariableAsOperand v
  addStatement $
    ASMSyntax.InstructionMOV v' (Left $ opRAX (ASMSyntax.operandType v'))
translateStatement (LinearSyntax.StatementAssignToPtr p v) = do
  p' <- resolveVariableAsOperand p
  addStatement $ ASMSyntax.InstructionMOV (opRAX ASMSyntax.VarTypeInt) (Left p')
  v' <- resolveVariableAsOperand v
  addStatement $
    ASMSyntax.InstructionMOV
      (ASMSyntax.OperandPointer
         ASMSyntax.Pointer
           { ASMSyntax.pointerType = ASMSyntax.operandType v'
           , ASMSyntax.pointerBase = Just ASMSyntax.RegisterRAX
           , ASMSyntax.pointerDisplacement = 0
           })
      (Left v')
translateStatement (LinearSyntax.StatementReturn Nothing) = do
  Nothing <- Reader.asks retValueLocation
  lbl <- Reader.asks epilogueLabel
  addStatement $ ASMSyntax.InstructionJMP lbl
translateStatement (LinearSyntax.StatementReturn (Just v)) = do
  v' <- resolveVariableAsOperand v
  Just rl <- Reader.asks retValueLocation
  addStatement $ ASMSyntax.InstructionMOV rl (Left v')
  lbl <- Reader.asks epilogueLabel
  addStatement $ ASMSyntax.InstructionJMP lbl
translateStatement (LinearSyntax.StatementLabel l) =
  addStatement $ ASMSyntax.InstructionLabelledNOP l
translateStatement (LinearSyntax.StatementJump l) =
  addStatement $ ASMSyntax.InstructionJMP l
translateStatement (LinearSyntax.StatementJumpIfZero v l) = do
  v' <- resolveVariableAsOperand v
  compareIntToZero v'
  addStatement $ ASMSyntax.InstructionJZ l

prepareArgsForCall ::
     CallingConvention -> [ASMSyntax.Operand] -> ASMStatement ()
prepareArgsForCall cc args = do
  mapM_
    (addStatement . ASMSyntax.StatementAllocateOnStack)
    (CallingConvention.funStackToAllocate cc)
  mapM_ go (zip args (CallingConvention.funArgValues cc))
  addStatement $
    ASMSyntax.InstructionMOV (opRAX ASMSyntax.VarTypeInt) $
    Right $
    ASMSyntax.ImmediateInt
      (fromIntegral $ CallingConvention.funFloatRegistersUsed cc)
  where
    go :: (ASMSyntax.Operand, CallingConvention.ArgLocation) -> ASMStatement ()
    go (arg, CallingConvention.ArgLocationRegister _ r) =
      addStatement $
      ASMSyntax.InstructionMOV
        (ASMSyntax.OperandRegister (ASMSyntax.operandType arg) r)
        (Left arg)
    go (arg, CallingConvention.ArgLocationStack t d) =
      addStatement $
      ASMSyntax.InstructionMOV
        (ASMSyntax.OperandPointer
           ASMSyntax.Pointer
             { ASMSyntax.pointerType = t
             , ASMSyntax.pointerBase = Just ASMSyntax.RegisterRSP
             , ASMSyntax.pointerDisplacement = -(d + 1)
             })
        (Left arg)

cleanStackAfterCall :: CallingConvention -> ASMStatement ()
cleanStackAfterCall cc =
  mapM_
    (addStatement . ASMSyntax.StatementPopFromStack)
    (reverse $ CallingConvention.funStackToAllocate cc)

prepareArgsAtCall :: [Var] -> CallingConvention -> ASMStatement ()
prepareArgsAtCall params cc = do
  let vals = map go (CallingConvention.funArgValues cc)
  generateAssignments params vals
  where
    go :: CallingConvention.ArgLocation -> ASMSyntax.Operand
    go (CallingConvention.ArgLocationRegister t r) =
      ASMSyntax.OperandRegister t r
    go (CallingConvention.ArgLocationStack t d) =
      ASMSyntax.OperandPointer
        ASMSyntax.Pointer
          { ASMSyntax.pointerType = t
          , ASMSyntax.pointerBase = Just ASMSyntax.RegisterRBP
          , ASMSyntax.pointerDisplacement = -(d + 3)
          }
    generateAssignments :: [Var] -> [ASMSyntax.Operand] -> ASMStatement ()
    generateAssignments [] [] = pure ()
    generateAssignments (Var {varType = t, varDisplacement = d}:vs) (val:vals) = do
      addStatement $
        ASMSyntax.InstructionMOV
          (ASMSyntax.OperandPointer
             ASMSyntax.Pointer
               { ASMSyntax.pointerType = t
               , ASMSyntax.pointerBase = Just ASMSyntax.RegisterRBP
               , ASMSyntax.pointerDisplacement = d
               })
          (Left val)
      generateAssignments vs vals
    generateAssignments _ _ = error "Type mismatch"

translateFunctionCall ::
     LinearSyntax.FunctionCall -> ASMStatement CallingConvention
translateFunctionCall fcall@LinearSyntax.NativeFunctionCall {} = do
  args <- mapM resolveVariableAsOperand $ LinearSyntax.nativeFunCallArgs fcall
  let cc =
        CallingConvention.computeCallingConvention
          CallingConvention.FunctionCall
            { CallingConvention.funRetType =
                LinearSyntax.nativeFunCallRetType fcall
            , CallingConvention.funArgTypes = map ASMSyntax.operandType args
            }
  prepareArgsForCall cc args
  let (LinearSyntax.FunID fid) = LinearSyntax.nativeFunCallName fcall
  flbl <- State.gets ((IntMap.! fid) . funIdToLabelID)
  addStatement $
    ASMSyntax.InstructionCALL $
    ASMSyntax.NativeFunctionCall {ASMSyntax.nativeFunCallName = flbl}
  cleanStackAfterCall cc
  pure cc
translateFunctionCall fcall@LinearSyntax.ForeignFunctionCall {} = do
  args <- mapM resolveVariableAsOperand $ LinearSyntax.foreignFunCallArgs fcall
  let cc =
        CallingConvention.computeCallingConvention
          CallingConvention.FunctionCall
            { CallingConvention.funRetType =
                LinearSyntax.foreignFunCallRetType fcall
            , CallingConvention.funArgTypes = map ASMSyntax.operandType args
            }
  prepareArgsForCall cc args
  addStatement $
    ASMSyntax.InstructionCALL $
    ASMSyntax.ForeignFunctionCall
      { ASMSyntax.foreignFunCallName = LinearSyntax.foreignFunCallName fcall
      , ASMSyntax.foreignFunCallRetType =
          LinearSyntax.foreignFunCallRetType fcall
      , ASMSyntax.foreignFunCallArgTypes = map ASMSyntax.operandType args
      }
  cleanStackAfterCall cc
  pure cc

translateExpr :: LinearSyntax.Expr -> ASMStatement ()
translateExpr (LinearSyntax.ExprFunctionCall _) =
  error "Must've been handled in translateStatement"
translateExpr (LinearSyntax.ExprVar v) = do
  v' <- resolveVariableAsOperand v
  addStatement $
    ASMSyntax.InstructionMOV (opRAX (ASMSyntax.operandType v')) (Left v')
translateExpr (LinearSyntax.ExprDereference v) = do
  v' <- resolveVariableAsOperand v
  addStatement $
    ASMSyntax.InstructionMOV (opRCX (ASMSyntax.operandType v')) (Left v')
  addStatement $
    ASMSyntax.InstructionMOV
      (opRAX (ASMSyntax.operandType v'))
      (Left $
       ASMSyntax.OperandPointer
         ASMSyntax.Pointer
           { ASMSyntax.pointerBase = Just ASMSyntax.RegisterRCX
           , ASMSyntax.pointerDisplacement = 0
           , ASMSyntax.pointerType = ASMSyntax.operandType v'
           })
translateExpr (LinearSyntax.ExprAddressOf v) = do
  Var {varDisplacement = d} <- resolveVariable v
  addStatement $
    ASMSyntax.InstructionMOV
      (opRCX ASMSyntax.VarTypeInt)
      (Right $ ASMSyntax.ImmediateInt d)
  let rax = opRAX ASMSyntax.VarTypeInt
  addStatement $ ASMSyntax.InstructionMOV rax (Left opRBP)
  addStatement $ ASMSyntax.InstructionADD rax (opRCX ASMSyntax.VarTypeInt)
translateExpr (LinearSyntax.ExprConst c) = do
  addStatement $
    ASMSyntax.InstructionMOV (opRAX (ASMSyntax.immediateType c)) (Right c)
translateExpr (LinearSyntax.ExprBinOp op lhs rhs) = do
  lhs' <- resolveVariableAsOperand lhs
  rhs' <- resolveVariableAsOperand rhs
  translateBinOp op lhs' rhs'
translateExpr (LinearSyntax.ExprUnOp op v) = do
  v' <- resolveVariableAsOperand v
  translateUnOp op v'

translateUnOp :: LinearSyntax.UnOp -> ASMSyntax.Operand -> ASMStatement ()
translateUnOp LinearSyntax.UnNeg v =
  case (ASMSyntax.operandType v) of
    ASMSyntax.VarTypeInt -> do
      addStatement $
        ASMSyntax.InstructionMOV (opRAX ASMSyntax.VarTypeInt) (Left v)
      addStatement $ ASMSyntax.InstructionNEG (opRAX ASMSyntax.VarTypeInt)
    ASMSyntax.VarTypeFloat ->
      addStatement $ ASMSyntax.StatementUnOp ASMSyntax.UnNegFloat v
    _ -> error "Type mismatch"
translateUnOp LinearSyntax.UnNot v = do
  compareIntToZero v
  addStatement $ ASMSyntax.InstructionSetZ (opRAX ASMSyntax.VarTypeInt)
translateUnOp LinearSyntax.UnIntToFloat v =
  addStatement $ ASMSyntax.StatementUnOp ASMSyntax.UnIntToFloat v

translateBinOp ::
     LinearSyntax.BinOp
  -> ASMSyntax.Operand
  -> ASMSyntax.Operand
  -> ASMStatement ()
translateBinOp LinearSyntax.BinPlus lhs rhs =
  case (ASMSyntax.operandType lhs, ASMSyntax.operandType rhs) of
    (ASMSyntax.VarTypeInt, ASMSyntax.VarTypeInt) -> do
      let lhs' = opRAX ASMSyntax.VarTypeInt
      addStatement $ ASMSyntax.InstructionMOV lhs' (Left lhs)
      addStatement $ ASMSyntax.InstructionADD lhs' rhs
    (ASMSyntax.VarTypeFloat, ASMSyntax.VarTypeFloat) ->
      addStatement $ ASMSyntax.StatementBinOp ASMSyntax.BinPlusFloat lhs rhs
    _ -> error "Type mismatch"
translateBinOp LinearSyntax.BinMinus lhs rhs =
  case (ASMSyntax.operandType lhs, ASMSyntax.operandType rhs) of
    (ASMSyntax.VarTypeInt, ASMSyntax.VarTypeInt) -> do
      let lhs' = opRAX ASMSyntax.VarTypeInt
      addStatement $ ASMSyntax.InstructionMOV lhs' (Left lhs)
      addStatement $ ASMSyntax.InstructionSUB lhs' rhs
    (ASMSyntax.VarTypeFloat, ASMSyntax.VarTypeFloat) ->
      addStatement $ ASMSyntax.StatementBinOp ASMSyntax.BinMinusFloat lhs rhs
    _ -> error "Type mismatch"
translateBinOp LinearSyntax.BinTimes lhs rhs =
  case (ASMSyntax.operandType lhs, ASMSyntax.operandType rhs) of
    (ASMSyntax.VarTypeInt, ASMSyntax.VarTypeInt) -> do
      let lhs' = opRAX ASMSyntax.VarTypeInt
      addStatement $ ASMSyntax.InstructionMOV lhs' (Left lhs)
      addStatement $ ASMSyntax.InstructionIMUL lhs' rhs
    (ASMSyntax.VarTypeFloat, ASMSyntax.VarTypeFloat) ->
      addStatement $ ASMSyntax.StatementBinOp ASMSyntax.BinTimesFloat lhs rhs
    _ -> error "Type mismatch"
translateBinOp LinearSyntax.BinDiv lhs rhs =
  case (ASMSyntax.operandType lhs, ASMSyntax.operandType rhs) of
    (ASMSyntax.VarTypeInt, ASMSyntax.VarTypeInt) -> do
      let lhs' = opRAX ASMSyntax.VarTypeInt
      addStatement $ ASMSyntax.InstructionMOV lhs' (Left lhs)
      addStatement $ ASMSyntax.InstructionCQO
      addStatement $ ASMSyntax.InstructionIDIV rhs
    (ASMSyntax.VarTypeFloat, ASMSyntax.VarTypeFloat) ->
      addStatement $ ASMSyntax.StatementBinOp ASMSyntax.BinDivFloat lhs rhs
    _ -> error "Type mismatch"
translateBinOp LinearSyntax.BinMod lhs rhs =
  case (ASMSyntax.operandType lhs, ASMSyntax.operandType rhs) of
    (ASMSyntax.VarTypeInt, ASMSyntax.VarTypeInt) -> do
      let lhs' = opRAX ASMSyntax.VarTypeInt
      addStatement $ ASMSyntax.InstructionMOV lhs' (Left lhs)
      addStatement $ ASMSyntax.InstructionCQO
      addStatement $ ASMSyntax.InstructionIDIV rhs
      addStatement $
        ASMSyntax.InstructionMOV lhs' (Left $ opRDX ASMSyntax.VarTypeInt)
    _ -> error "Type mismatch"
translateBinOp LinearSyntax.BinBitAnd lhs rhs = do
  let lhs' = opRAX ASMSyntax.VarTypeInt
  addStatement $ ASMSyntax.InstructionMOV lhs' (Left lhs)
  addStatement $ ASMSyntax.InstructionAND lhs' rhs
translateBinOp LinearSyntax.BinBitOr lhs rhs = do
  let lhs' = opRAX ASMSyntax.VarTypeInt
  addStatement $ ASMSyntax.InstructionMOV lhs' (Left lhs)
  addStatement $ ASMSyntax.InstructionOR lhs' rhs
translateBinOp LinearSyntax.BinBitXor lhs rhs = do
  let lhs' = opRAX ASMSyntax.VarTypeInt
  addStatement $ ASMSyntax.InstructionMOV lhs' (Left lhs)
  addStatement $ ASMSyntax.InstructionXOR lhs' rhs
translateBinOp LinearSyntax.BinAnd lhs rhs = do
  compareIntToZero lhs
  let lhs' = opRCX ASMSyntax.VarTypeInt
  addStatement $ ASMSyntax.InstructionSetNZ lhs'
  compareIntToZero rhs
  let rhs' = opRDX ASMSyntax.VarTypeInt
  addStatement $ ASMSyntax.InstructionSetNZ rhs'
  let lhs'' = opRAX ASMSyntax.VarTypeInt
  addStatement $ ASMSyntax.InstructionMOV lhs'' (Left lhs')
  addStatement $ ASMSyntax.InstructionAND lhs'' rhs'
translateBinOp LinearSyntax.BinOr lhs rhs = do
  compareIntToZero lhs
  let lhs' = opRCX ASMSyntax.VarTypeInt
  addStatement $ ASMSyntax.InstructionSetNZ lhs'
  compareIntToZero rhs
  let rhs' = opRDX ASMSyntax.VarTypeInt
  addStatement $ ASMSyntax.InstructionSetNZ rhs'
  let lhs'' = opRAX ASMSyntax.VarTypeInt
  addStatement $ ASMSyntax.InstructionMOV lhs'' (Left lhs')
  addStatement $ ASMSyntax.InstructionOR lhs'' rhs'
translateBinOp LinearSyntax.BinEq lhs rhs =
  case (ASMSyntax.operandType lhs, ASMSyntax.operandType rhs) of
    (ASMSyntax.VarTypeInt, ASMSyntax.VarTypeInt) -> do
      addStatement $ ASMSyntax.InstructionCMP lhs rhs
      addStatement $ ASMSyntax.InstructionSetZ (opRAX ASMSyntax.VarTypeInt)
    (ASMSyntax.VarTypeFloat, ASMSyntax.VarTypeFloat) ->
      addStatement $ ASMSyntax.StatementBinOp ASMSyntax.BinEqFloat lhs rhs
    _ -> error "Type mismatch"
translateBinOp LinearSyntax.BinLt lhs rhs =
  case (ASMSyntax.operandType lhs, ASMSyntax.operandType rhs) of
    (ASMSyntax.VarTypeInt, ASMSyntax.VarTypeInt) -> do
      addStatement $ ASMSyntax.InstructionCMP lhs rhs
      addStatement $ ASMSyntax.InstructionSetS (opRAX ASMSyntax.VarTypeInt)
    (ASMSyntax.VarTypeFloat, ASMSyntax.VarTypeFloat) ->
      addStatement $ ASMSyntax.StatementBinOp ASMSyntax.BinLtFloat lhs rhs
    _ -> error "Type mismatch"

-- Compare int operand to zero and set EFLAGS.
compareIntToZero :: ASMSyntax.Operand -> ASMStatement ()
compareIntToZero v = do
  addStatement $
    ASMSyntax.InstructionMOV
      (opRAX ASMSyntax.VarTypeInt)
      (Right $ ASMSyntax.ImmediateInt 0)
  addStatement $ ASMSyntax.InstructionCMP v (opRAX ASMSyntax.VarTypeInt)
