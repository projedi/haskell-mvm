module ASM
  ( avenge
  ) where

import Control.Monad.State (State, runState)
import qualified Control.Monad.State as State
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

resolveVariable :: LinearSyntax.Var -> ASM ASMSyntax.Var
resolveVariable LinearSyntax.Var {LinearSyntax.varName = LinearSyntax.VarID vid} =
  State.gets ((IntMap.! vid) . varMap)

resolveVariableAsOperand :: LinearSyntax.Var -> ASM ASMSyntax.Operand
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
  body <- mapM translateStatement $ LinearSyntax.funDefBody fdef
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

translateStatement :: LinearSyntax.Statement -> ASM ASMSyntax.Statement
translateStatement (LinearSyntax.StatementFunctionCall fcall) =
  ASMSyntax.StatementFunctionCall <$> translateFunctionCall fcall
translateStatement (LinearSyntax.StatementAssign v e) =
  ASMSyntax.StatementAssign <$> resolveVariableAsOperand v <*> translateExpr e
translateStatement (LinearSyntax.StatementAssignToPtr p v) =
  ASMSyntax.StatementAssignToPtr <$> resolveVariableAsOperand p <*>
  resolveVariableAsOperand v
translateStatement (LinearSyntax.StatementReturn Nothing) =
  pure $ ASMSyntax.StatementReturn Nothing
translateStatement (LinearSyntax.StatementReturn (Just v)) =
  (ASMSyntax.StatementReturn . Just) <$> resolveVariableAsOperand v
translateStatement (LinearSyntax.StatementLabel l) =
  pure $ ASMSyntax.StatementLabel l
translateStatement (LinearSyntax.StatementJump l) =
  pure $ ASMSyntax.StatementJump l
translateStatement (LinearSyntax.StatementJumpIfZero v l) = do
  v' <- resolveVariableAsOperand v
  pure $ ASMSyntax.StatementJumpIfZero v' l

translateFunctionCall :: LinearSyntax.FunctionCall -> ASM ASMSyntax.FunctionCall
translateFunctionCall fcall@LinearSyntax.NativeFunctionCall {} = do
  args <- mapM resolveVariableAsOperand $ LinearSyntax.nativeFunCallArgs fcall
  pure $
    ASMSyntax.NativeFunctionCall
      { ASMSyntax.nativeFunCallName = LinearSyntax.nativeFunCallName fcall
      , ASMSyntax.nativeFunCallRetType = LinearSyntax.nativeFunCallRetType fcall
      , ASMSyntax.nativeFunCallArgs = args
      }
translateFunctionCall fcall@LinearSyntax.ForeignFunctionCall {} = do
  args <- mapM resolveVariableAsOperand $ LinearSyntax.foreignFunCallArgs fcall
  pure $
    ASMSyntax.ForeignFunctionCall
      { ASMSyntax.foreignFunCallName = LinearSyntax.foreignFunCallName fcall
      , ASMSyntax.foreignFunCallRetType =
          LinearSyntax.foreignFunCallRetType fcall
      , ASMSyntax.foreignFunCallArgs = args
      }

translateExpr :: LinearSyntax.Expr -> ASM ASMSyntax.Expr
translateExpr (LinearSyntax.ExprFunctionCall fcall) =
  ASMSyntax.ExprFunctionCall <$> translateFunctionCall fcall
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