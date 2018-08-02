{-# LANGUAGE FlexibleContexts #-}

module ASM
  ( avenge
  ) where

import Control.Monad.State (MonadState, State, runState)
import qualified Control.Monad.State as State
import Control.Monad.Writer (WriterT, execWriterT)
import qualified Control.Monad.Writer as Writer
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import qualified ASMSyntax
import qualified LinearSyntax

avenge :: LinearSyntax.Program -> ASMSyntax.Program
avenge p =
  ASMSyntax.Program
    { ASMSyntax.programFunctions = fs
    , ASMSyntax.programLibraries = LinearSyntax.programLibraries p
    , ASMSyntax.programForeignFunctions = LinearSyntax.programForeignFunctions p
    , ASMSyntax.programConstants = LinearSyntax.programConstants p
    , ASMSyntax.programLastFunID = LinearSyntax.programLastFunID p
    , ASMSyntax.programLastConstID = LinearSyntax.programLastConstID p
    , ASMSyntax.programLastLabelID = LinearSyntax.programLastLabelID p
    }
  where
    (fs, _) =
      runState (mapM translateFunctionDef (LinearSyntax.programFunctions p)) $
      Env {varMap = IntMap.empty, varStack = []}

data Env = Env
  { varMap :: IntMap ASMSyntax.Var
  , varStack :: [ASMSyntax.VarType]
  }

type ASM = State Env

opRBP :: ASMSyntax.Operand
opRBP = ASMSyntax.OperandRegister ASMSyntax.VarTypeInt ASMSyntax.RegisterRBP

opRSP :: ASMSyntax.Operand
opRSP = ASMSyntax.OperandRegister ASMSyntax.VarTypeInt ASMSyntax.RegisterRSP

peekStack :: ASMSyntax.VarType -> ASMSyntax.Expr
peekStack t =
  ASMSyntax.ExprRead $
  ASMSyntax.OperandPointer
    ASMSyntax.Pointer
      { ASMSyntax.pointerType = t
      , ASMSyntax.pointerBase = Just ASMSyntax.RegisterRSP
      , ASMSyntax.pointerDisplacement = -1
      }

introduceVariable :: LinearSyntax.Var -> ASM ASMSyntax.Var
introduceVariable LinearSyntax.Var { LinearSyntax.varName = LinearSyntax.VarID vid
                                   , LinearSyntax.varType = t
                                   } = do
  d <- State.gets (fromIntegral . length . varStack)
  let var = ASMSyntax.Var {ASMSyntax.varType = t, ASMSyntax.varDisplacement = d}
  State.modify $ \env ->
    env
      {varMap = IntMap.insert vid var $ varMap env, varStack = t : varStack env}
  pure var

resolveVariable :: MonadState Env m => LinearSyntax.Var -> m ASMSyntax.Var
resolveVariable LinearSyntax.Var {LinearSyntax.varName = LinearSyntax.VarID vid} =
  State.gets ((IntMap.! vid) . varMap)

resolveVariableAsOperand ::
     MonadState Env m => LinearSyntax.Var -> m ASMSyntax.Operand
resolveVariableAsOperand v = do
  ASMSyntax.Var {ASMSyntax.varType = t, ASMSyntax.varDisplacement = d} <-
    resolveVariable v
  pure $
    ASMSyntax.OperandPointer
      ASMSyntax.Pointer
        { ASMSyntax.pointerType = t
        , ASMSyntax.pointerBase = Just ASMSyntax.RegisterRBP
        , ASMSyntax.pointerDisplacement = d
        }

translateFunctionDef :: LinearSyntax.FunctionDef -> ASM ASMSyntax.FunctionDef
translateFunctionDef fdef = do
  State.modify $ \env -> env {varStack = []}
  params <- mapM introduceVariable $ LinearSyntax.funDefParams fdef
  locals <- mapM introduceVariable $ LinearSyntax.funDefLocals fdef
  State.modify $ \env -> env {varStack = []}
  body <- execWriterT $ mapM_ translateStatement $ LinearSyntax.funDefBody fdef
  let saveRBP =
        [ ASMSyntax.StatementPushOnStack opRBP
        , ASMSyntax.StatementAssign opRBP (ASMSyntax.ExprRead opRSP)
        ]
  let declareVars =
        map (ASMSyntax.StatementAllocateOnStack . ASMSyntax.varType) $
        (params ++ locals)
  let undeclareVars =
        map (ASMSyntax.StatementPopFromStack . ASMSyntax.varType) $
        reverse (params ++ locals)
  let restoreRBP =
        [ ASMSyntax.StatementAssign opRBP (peekStack ASMSyntax.VarTypeInt)
        , ASMSyntax.StatementPopFromStack ASMSyntax.VarTypeInt
        ]
  pure $
    ASMSyntax.FunctionDef
      { ASMSyntax.funDefRetType = LinearSyntax.funDefRetType fdef
      , ASMSyntax.funDefName = LinearSyntax.funDefName fdef
      , ASMSyntax.funDefParams = params
      , ASMSyntax.funDefBody = body
      , ASMSyntax.funDefBeforeBody = saveRBP ++ declareVars
      , ASMSyntax.funDefAfterBody = undeclareVars ++ restoreRBP
      }

type ASMStatement = WriterT [ASMSyntax.Statement] ASM

addStatement :: ASMSyntax.Statement -> ASMStatement ()
addStatement s = Writer.tell [s]

translateStatement :: LinearSyntax.Statement -> ASMStatement ()
translateStatement (LinearSyntax.StatementFunctionCall fcall) = do
  translateFunctionCall Nothing fcall
translateStatement (LinearSyntax.StatementAssign v (LinearSyntax.ExprFunctionCall fcall)) =
  translateFunctionCall (Just v) fcall
translateStatement (LinearSyntax.StatementAssign v e) = do
  v' <- resolveVariableAsOperand v
  e' <- translateExpr e
  addStatement $ ASMSyntax.StatementAssign v' e'
translateStatement (LinearSyntax.StatementAssignToPtr p v) = do
  p' <- resolveVariableAsOperand p
  v' <- resolveVariableAsOperand v
  addStatement $ ASMSyntax.StatementAssignToPtr p' v'
translateStatement (LinearSyntax.StatementReturn Nothing) =
  addStatement $ ASMSyntax.StatementReturn Nothing
translateStatement (LinearSyntax.StatementReturn (Just v)) = do
  v' <- resolveVariableAsOperand v
  addStatement $ ASMSyntax.StatementReturn $ Just v'
translateStatement (LinearSyntax.StatementLabel l) =
  addStatement $ ASMSyntax.StatementLabel l
translateStatement (LinearSyntax.StatementJump l) =
  addStatement $ ASMSyntax.StatementJump l
translateStatement (LinearSyntax.StatementJumpIfZero v l) = do
  v' <- resolveVariableAsOperand v
  addStatement $ ASMSyntax.StatementJumpIfZero v' l

translateFunctionCall ::
     Maybe LinearSyntax.Var -> LinearSyntax.FunctionCall -> ASMStatement ()
translateFunctionCall mv fcall@LinearSyntax.NativeFunctionCall {} = do
  mv' <-
    case mv of
      Nothing -> pure Nothing
      Just v -> Just <$> resolveVariableAsOperand v
  args <- mapM resolveVariableAsOperand $ LinearSyntax.nativeFunCallArgs fcall
  addStatement $
    ASMSyntax.StatementFunctionCall mv' $
    ASMSyntax.NativeFunctionCall
      { ASMSyntax.nativeFunCallName = LinearSyntax.nativeFunCallName fcall
      , ASMSyntax.nativeFunCallRetType = LinearSyntax.nativeFunCallRetType fcall
      , ASMSyntax.nativeFunCallArgs = args
      }
translateFunctionCall mv fcall@LinearSyntax.ForeignFunctionCall {} = do
  mv' <-
    case mv of
      Nothing -> pure Nothing
      Just v -> Just <$> resolveVariableAsOperand v
  args <- mapM resolveVariableAsOperand $ LinearSyntax.foreignFunCallArgs fcall
  addStatement $
    ASMSyntax.StatementFunctionCall mv' $
    ASMSyntax.ForeignFunctionCall
      { ASMSyntax.foreignFunCallName = LinearSyntax.foreignFunCallName fcall
      , ASMSyntax.foreignFunCallRetType =
          LinearSyntax.foreignFunCallRetType fcall
      , ASMSyntax.foreignFunCallArgs = args
      }

translateExpr :: LinearSyntax.Expr -> ASMStatement ASMSyntax.Expr
translateExpr (LinearSyntax.ExprFunctionCall _) =
  error "Must've been handled in translateStatement"
translateExpr (LinearSyntax.ExprVar v) =
  ASMSyntax.ExprRead <$> resolveVariableAsOperand v
translateExpr (LinearSyntax.ExprDereference v) =
  ASMSyntax.ExprDereference <$> resolveVariableAsOperand v
translateExpr (LinearSyntax.ExprAddressOf v) = do
  ASMSyntax.Var {ASMSyntax.varDisplacement = d} <- resolveVariable v
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
