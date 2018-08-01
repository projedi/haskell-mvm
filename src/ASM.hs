module ASM
  ( avenge
  ) where

import Control.Monad.State (State, runState)

import qualified ASMSyntax
import qualified LinearSyntax

avenge :: LinearSyntax.Program -> ASMSyntax.Program
avenge p =
  ASMSyntax.Program
    { ASMSyntax.programFunctions = fs
    , ASMSyntax.programLibraries = LinearSyntax.programLibraries p
    , ASMSyntax.programForeignFunctions = LinearSyntax.programForeignFunctions p
    , ASMSyntax.programConstants = LinearSyntax.programConstants p
    , ASMSyntax.programVariables = LinearSyntax.programVariables p
    , ASMSyntax.programLastFunID = LinearSyntax.programLastFunID p
    , ASMSyntax.programLastVarID = LinearSyntax.programLastVarID p
    , ASMSyntax.programLastConstID = LinearSyntax.programLastConstID p
    , ASMSyntax.programLastLabelID = LinearSyntax.programLastLabelID p
    }
  where
    (fs, _) =
      runState (mapM translateFunctionDef (LinearSyntax.programFunctions p)) Env

data Env =
  Env

type ASM = State Env

translateFunctionDef :: LinearSyntax.FunctionDef -> ASM ASMSyntax.FunctionDef
translateFunctionDef fdef = do
  body <- mapM translateStatement $ LinearSyntax.funDefBody fdef
  pure $
    ASMSyntax.FunctionDef
      { ASMSyntax.funDefRetType = LinearSyntax.funDefRetType fdef
      , ASMSyntax.funDefName = LinearSyntax.funDefName fdef
      , ASMSyntax.funDefParams = LinearSyntax.funDefParams fdef
      , ASMSyntax.funDefLocals = LinearSyntax.funDefLocals fdef
      , ASMSyntax.funDefBody = body
      }

translateStatement :: LinearSyntax.Statement -> ASM ASMSyntax.Statement
translateStatement (LinearSyntax.StatementFunctionCall fcall) =
  ASMSyntax.StatementFunctionCall <$> translateFunctionCall fcall
translateStatement (LinearSyntax.StatementAssign v e) =
  ASMSyntax.StatementAssign v <$> translateExpr e
translateStatement (LinearSyntax.StatementAssignToPtr p v) =
  pure $ ASMSyntax.StatementAssignToPtr p v
translateStatement (LinearSyntax.StatementReturn mv) =
  pure $ ASMSyntax.StatementReturn mv
translateStatement (LinearSyntax.StatementLabel l) =
  pure $ ASMSyntax.StatementLabel l
translateStatement (LinearSyntax.StatementJump l) =
  pure $ ASMSyntax.StatementJump l
translateStatement (LinearSyntax.StatementJumpIfZero v l) =
  pure $ ASMSyntax.StatementJumpIfZero v l

translateFunctionCall :: LinearSyntax.FunctionCall -> ASM ASMSyntax.FunctionCall
translateFunctionCall fcall@LinearSyntax.NativeFunctionCall {} =
  pure $
  ASMSyntax.NativeFunctionCall
    { ASMSyntax.nativeFunCallName = LinearSyntax.nativeFunCallName fcall
    , ASMSyntax.nativeFunCallRetType = LinearSyntax.nativeFunCallRetType fcall
    , ASMSyntax.nativeFunCallArgs = LinearSyntax.nativeFunCallArgs fcall
    }
translateFunctionCall fcall@LinearSyntax.ForeignFunctionCall {} =
  pure $
  ASMSyntax.ForeignFunctionCall
    { ASMSyntax.foreignFunCallName = LinearSyntax.foreignFunCallName fcall
    , ASMSyntax.foreignFunCallRetType = LinearSyntax.foreignFunCallRetType fcall
    , ASMSyntax.foreignFunCallArgs = LinearSyntax.foreignFunCallArgs fcall
    }

translateExpr :: LinearSyntax.Expr -> ASM ASMSyntax.Expr
translateExpr (LinearSyntax.ExprFunctionCall fcall) =
  ASMSyntax.ExprFunctionCall <$> translateFunctionCall fcall
translateExpr (LinearSyntax.ExprVar v) = pure $ ASMSyntax.ExprVar v
translateExpr (LinearSyntax.ExprDereference v) =
  pure $ ASMSyntax.ExprDereference v
translateExpr (LinearSyntax.ExprAddressOf v) = pure $ ASMSyntax.ExprAddressOf v
translateExpr (LinearSyntax.ExprConst t c) = pure $ ASMSyntax.ExprConst t c
translateExpr (LinearSyntax.ExprBinOp op lhs rhs) =
  pure $ ASMSyntax.ExprBinOp op lhs rhs
translateExpr (LinearSyntax.ExprUnOp op v) = pure $ ASMSyntax.ExprUnOp op v
