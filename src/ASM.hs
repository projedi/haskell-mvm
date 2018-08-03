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
    , ASMSyntax.programConstants = LinearSyntax.programConstants p
    , ASMSyntax.programLastFunID = LinearSyntax.programLastFunID p
    , ASMSyntax.programLastConstID = LinearSyntax.programLastConstID p
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

peekStack :: ASMSyntax.VarType -> ASMSyntax.Expr
peekStack t =
  ASMSyntax.ExprRead $
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
    ASMSyntax.StatementFunctionCall $
    ASMSyntax.NativeFunctionCall
      {ASMSyntax.nativeFunCallName = funMap IntMap.! 0}
  addStatement $ ASMSyntax.StatementReturn
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
    addStatement $ ASMSyntax.StatementLabel funLbl
    let stackVars = params ++ locals
    addStatement $ ASMSyntax.StatementPushOnStack opRBP
    addStatement $ ASMSyntax.StatementAssign opRBP (ASMSyntax.ExprRead opRSP)
    mapM_
      (addStatement . ASMSyntax.StatementAllocateOnStack . varType)
      stackVars
    prepareArgsAtCall params cc
    mapM_ translateStatement $ LinearSyntax.funDefBody fdef
    addStatement $ ASMSyntax.StatementLabel epilogueLbl
    mapM_ (addStatement . ASMSyntax.StatementPopFromStack . varType) $
      reverse stackVars
    addStatement $
      ASMSyntax.StatementAssign opRBP (peekStack ASMSyntax.VarTypeInt)
    addStatement $ ASMSyntax.StatementPopFromStack ASMSyntax.VarTypeInt
    addStatement $ ASMSyntax.StatementReturn

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
  addStatement $ ASMSyntax.StatementAssign v' $ ASMSyntax.ExprRead retValue
translateStatement (LinearSyntax.StatementAssign v e) = do
  v' <- resolveVariableAsOperand v
  e' <- translateExpr e
  addStatement $ ASMSyntax.StatementAssign v' e'
translateStatement (LinearSyntax.StatementAssignToPtr p v) = do
  p' <- resolveVariableAsOperand p
  v' <- resolveVariableAsOperand v
  addStatement $ ASMSyntax.StatementAssignToPtr p' v'
translateStatement (LinearSyntax.StatementReturn Nothing) = do
  Nothing <- Reader.asks retValueLocation
  lbl <- Reader.asks epilogueLabel
  addStatement $ ASMSyntax.StatementJump lbl
translateStatement (LinearSyntax.StatementReturn (Just v)) = do
  v' <- resolveVariableAsOperand v
  Just rl <- Reader.asks retValueLocation
  addStatement $ ASMSyntax.StatementAssign rl $ ASMSyntax.ExprRead v'
  lbl <- Reader.asks epilogueLabel
  addStatement $ ASMSyntax.StatementJump lbl
  addStatement $ ASMSyntax.StatementReturn
translateStatement (LinearSyntax.StatementLabel l) =
  addStatement $ ASMSyntax.StatementLabel l
translateStatement (LinearSyntax.StatementJump l) =
  addStatement $ ASMSyntax.StatementJump l
translateStatement (LinearSyntax.StatementJumpIfZero v l) = do
  v' <- resolveVariableAsOperand v
  addStatement $ ASMSyntax.StatementJumpIfZero v' l

prepareArgsForCall ::
     CallingConvention -> [ASMSyntax.Operand] -> ASMStatement ()
prepareArgsForCall cc args = do
  mapM_
    (addStatement . ASMSyntax.StatementAllocateOnStack)
    (CallingConvention.funStackToAllocate cc)
  mapM_ go (zip args (CallingConvention.funArgValues cc))
  addStatement $
    ASMSyntax.StatementAssign (opRAX ASMSyntax.VarTypeInt) $
    ASMSyntax.ExprRead $
    ASMSyntax.OperandImmediateInt
      (fromIntegral $ CallingConvention.funFloatRegistersUsed cc)
  where
    go :: (ASMSyntax.Operand, CallingConvention.ArgLocation) -> ASMStatement ()
    go (arg, CallingConvention.ArgLocationRegister _ r) =
      addStatement $
      ASMSyntax.StatementAssign
        (ASMSyntax.OperandRegister (ASMSyntax.operandType arg) r)
        (ASMSyntax.ExprRead arg)
    go (arg, CallingConvention.ArgLocationStack t d) =
      addStatement $
      ASMSyntax.StatementAssign
        (ASMSyntax.OperandPointer
           ASMSyntax.Pointer
             { ASMSyntax.pointerType = t
             , ASMSyntax.pointerBase = Just ASMSyntax.RegisterRSP
             , ASMSyntax.pointerDisplacement = -(d + 1)
             })
        (ASMSyntax.ExprRead arg)

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
        ASMSyntax.StatementAssign
          (ASMSyntax.OperandPointer
             ASMSyntax.Pointer
               { ASMSyntax.pointerType = t
               , ASMSyntax.pointerBase = Just ASMSyntax.RegisterRBP
               , ASMSyntax.pointerDisplacement = d
               })
          (ASMSyntax.ExprRead val)
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
    ASMSyntax.StatementFunctionCall $
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
    ASMSyntax.StatementFunctionCall $
    ASMSyntax.ForeignFunctionCall
      { ASMSyntax.foreignFunCallName = LinearSyntax.foreignFunCallName fcall
      , ASMSyntax.foreignFunCallRetType =
          LinearSyntax.foreignFunCallRetType fcall
      , ASMSyntax.foreignFunCallArgTypes = map ASMSyntax.operandType args
      }
  cleanStackAfterCall cc
  pure cc

translateExpr :: LinearSyntax.Expr -> ASMStatement ASMSyntax.Expr
translateExpr (LinearSyntax.ExprFunctionCall _) =
  error "Must've been handled in translateStatement"
translateExpr (LinearSyntax.ExprVar v) =
  ASMSyntax.ExprRead <$> resolveVariableAsOperand v
translateExpr (LinearSyntax.ExprDereference v) =
  ASMSyntax.ExprDereference <$> resolveVariableAsOperand v
translateExpr (LinearSyntax.ExprAddressOf v) = do
  Var {varDisplacement = d} <- resolveVariable v
  pure $
    ASMSyntax.ExprBinOp
      ASMSyntax.BinPlus
      opRBP
      (ASMSyntax.OperandImmediateInt d)
translateExpr (LinearSyntax.ExprConst t c) = pure $ ASMSyntax.ExprConst t c
translateExpr (LinearSyntax.ExprBinOp op lhs rhs) =
  ASMSyntax.ExprBinOp op <$> resolveVariableAsOperand lhs <*>
  resolveVariableAsOperand rhs
translateExpr (LinearSyntax.ExprUnOp op v) =
  ASMSyntax.ExprUnOp op <$> resolveVariableAsOperand v
