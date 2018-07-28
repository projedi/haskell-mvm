module ASM
  ( avenge
  ) where

import qualified ASMSyntax
import qualified LinearSyntax

avenge :: LinearSyntax.Program -> ASMSyntax.Program
avenge p =
  ASMSyntax.Program
    { ASMSyntax.programFunctions =
        translateFunctionDef <$> LinearSyntax.programFunctions p
    , ASMSyntax.programLibraries = LinearSyntax.programLibraries p
    , ASMSyntax.programForeignFunctions = LinearSyntax.programForeignFunctions p
    , ASMSyntax.programConstants = LinearSyntax.programConstants p
    , ASMSyntax.programVariables = LinearSyntax.programVariables p
    , ASMSyntax.programLastFunID = LinearSyntax.programLastFunID p
    , ASMSyntax.programLastVarID = LinearSyntax.programLastVarID p
    , ASMSyntax.programLastConstID = LinearSyntax.programLastConstID p
    , ASMSyntax.programLastLabelID = LinearSyntax.programLastLabelID p
    }

translateFunctionDef :: LinearSyntax.FunctionDef -> ASMSyntax.FunctionDef
translateFunctionDef fdef =
  ASMSyntax.FunctionDef
    { ASMSyntax.funDefRetType = LinearSyntax.funDefRetType fdef
    , ASMSyntax.funDefName = LinearSyntax.funDefName fdef
    , ASMSyntax.funDefParams = LinearSyntax.funDefParams fdef
    , ASMSyntax.funDefLocals = LinearSyntax.funDefLocals fdef
    , ASMSyntax.funDefBody = translateStatement <$> LinearSyntax.funDefBody fdef
    }

translateStatement :: LinearSyntax.Statement -> ASMSyntax.Statement
translateStatement (LinearSyntax.StatementFunctionCall fcall) =
  ASMSyntax.StatementFunctionCall $ translateFunctionCall fcall
translateStatement (LinearSyntax.StatementAssign v e) =
  ASMSyntax.StatementAssign v $ translateExpr e
translateStatement (LinearSyntax.StatementAssignToPtr p v) =
  ASMSyntax.StatementAssignToPtr p v
translateStatement (LinearSyntax.StatementReturn mv) =
  ASMSyntax.StatementReturn mv
translateStatement (LinearSyntax.StatementLabel l) = ASMSyntax.StatementLabel l
translateStatement (LinearSyntax.StatementJump l) = ASMSyntax.StatementJump l
translateStatement (LinearSyntax.StatementJumpIfZero v l) =
  ASMSyntax.StatementJumpIfZero v l

translateFunctionCall :: LinearSyntax.FunctionCall -> ASMSyntax.FunctionCall
translateFunctionCall fcall@LinearSyntax.NativeFunctionCall {} =
  ASMSyntax.NativeFunctionCall
    { ASMSyntax.nativeFunCallName = LinearSyntax.nativeFunCallName fcall
    , ASMSyntax.nativeFunCallRetType = LinearSyntax.nativeFunCallRetType fcall
    , ASMSyntax.nativeFunCallArgs = LinearSyntax.nativeFunCallArgs fcall
    }
translateFunctionCall fcall@LinearSyntax.ForeignFunctionCall {} =
  ASMSyntax.ForeignFunctionCall
    { ASMSyntax.foreignFunCallName = LinearSyntax.foreignFunCallName fcall
    , ASMSyntax.foreignFunCallRetType = LinearSyntax.foreignFunCallRetType fcall
    , ASMSyntax.foreignFunCallArgs = LinearSyntax.foreignFunCallArgs fcall
    }

translateExpr :: LinearSyntax.Expr -> ASMSyntax.Expr
translateExpr (LinearSyntax.ExprFunctionCall fcall) =
  ASMSyntax.ExprFunctionCall $ translateFunctionCall fcall
translateExpr (LinearSyntax.ExprVar v) = ASMSyntax.ExprVar v
translateExpr (LinearSyntax.ExprDereference v) = ASMSyntax.ExprDereference v
translateExpr (LinearSyntax.ExprAddressOf v) = ASMSyntax.ExprAddressOf v
translateExpr (LinearSyntax.ExprConst t c) = ASMSyntax.ExprConst t c
translateExpr (LinearSyntax.ExprBinOp op lhs rhs) =
  ASMSyntax.ExprBinOp op lhs rhs
translateExpr (LinearSyntax.ExprUnOp op v) = ASMSyntax.ExprUnOp op v
