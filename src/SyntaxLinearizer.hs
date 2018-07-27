module SyntaxLinearizer
  ( linearize
  ) where

import qualified LinearSyntax
import qualified SimplifiedSyntax

linearize :: SimplifiedSyntax.Program -> LinearSyntax.Program
linearize p =
  LinearSyntax.Program
    { LinearSyntax.programFunctions =
        linearizeFunctionDef <$> SimplifiedSyntax.programFunctions p
    , LinearSyntax.programLibraries = SimplifiedSyntax.programLibraries p
    , LinearSyntax.programForeignFunctions =
        SimplifiedSyntax.programForeignFunctions p
    , LinearSyntax.programConstants = SimplifiedSyntax.programConstants p
    , LinearSyntax.programVariables = SimplifiedSyntax.programVariables p
    , LinearSyntax.programLastFunID = SimplifiedSyntax.programLastFunID p
    , LinearSyntax.programLastVarID = SimplifiedSyntax.programLastVarID p
    , LinearSyntax.programLastConstID = SimplifiedSyntax.programLastConstID p
    }

linearizeFunctionDef :: SimplifiedSyntax.FunctionDef -> LinearSyntax.FunctionDef
linearizeFunctionDef f =
  LinearSyntax.FunctionDef
    { LinearSyntax.funDefRetType = SimplifiedSyntax.funDefRetType f
    , LinearSyntax.funDefName = SimplifiedSyntax.funDefName f
    , LinearSyntax.funDefParams = SimplifiedSyntax.funDefParams f
    , LinearSyntax.funDefBody = linearizeBlock $ SimplifiedSyntax.funDefBody f
    }

linearizeBlock :: SimplifiedSyntax.Block -> LinearSyntax.Block
linearizeBlock b =
  LinearSyntax.Block
    { LinearSyntax.blockStatements =
        linearizeStatement <$> SimplifiedSyntax.blockStatements b
    }

linearizeStatement :: SimplifiedSyntax.Statement -> LinearSyntax.Statement
linearizeStatement (SimplifiedSyntax.StatementBlock b) =
  LinearSyntax.StatementBlock $ linearizeBlock b
linearizeStatement (SimplifiedSyntax.StatementVarAlloc v) =
  LinearSyntax.StatementVarAlloc v
linearizeStatement (SimplifiedSyntax.StatementFunctionCall fcall) =
  LinearSyntax.StatementFunctionCall $ linearizeFunctionCall fcall
linearizeStatement (SimplifiedSyntax.StatementWhile e b) =
  LinearSyntax.StatementWhile (linearizeExpr e) (linearizeBlock b)
linearizeStatement (SimplifiedSyntax.StatementAssign v e) =
  LinearSyntax.StatementAssign v (linearizeExpr e)
linearizeStatement (SimplifiedSyntax.StatementAssignToPtr v e) =
  LinearSyntax.StatementAssignToPtr v (linearizeExpr e)
linearizeStatement (SimplifiedSyntax.StatementIfElse e tb fb) =
  LinearSyntax.StatementIfElse
    (linearizeExpr e)
    (linearizeBlock tb)
    (linearizeBlock fb)
linearizeStatement (SimplifiedSyntax.StatementReturn me) =
  LinearSyntax.StatementReturn (linearizeExpr <$> me)

linearizeFunctionCall ::
     SimplifiedSyntax.FunctionCall -> LinearSyntax.FunctionCall
linearizeFunctionCall fcall@SimplifiedSyntax.NativeFunctionCall {} =
  LinearSyntax.NativeFunctionCall
    { LinearSyntax.nativeFunCallName = SimplifiedSyntax.nativeFunCallName fcall
    , LinearSyntax.nativeFunCallRetType =
        SimplifiedSyntax.nativeFunCallRetType fcall
    , LinearSyntax.nativeFunCallArgs =
        linearizeExpr <$> SimplifiedSyntax.nativeFunCallArgs fcall
    }
linearizeFunctionCall fcall@SimplifiedSyntax.ForeignFunctionCall {} =
  LinearSyntax.ForeignFunctionCall
    { LinearSyntax.foreignFunCallName =
        SimplifiedSyntax.foreignFunCallName fcall
    , LinearSyntax.foreignFunCallRetType =
        SimplifiedSyntax.foreignFunCallRetType fcall
    , LinearSyntax.foreignFunCallArgs =
        linearizeExpr <$> SimplifiedSyntax.foreignFunCallArgs fcall
    }

linearizeExpr :: SimplifiedSyntax.Expr -> LinearSyntax.Expr
linearizeExpr (SimplifiedSyntax.ExprFunctionCall fcall) =
  LinearSyntax.ExprFunctionCall (linearizeFunctionCall fcall)
linearizeExpr (SimplifiedSyntax.ExprVar t v) = LinearSyntax.ExprVar t v
linearizeExpr (SimplifiedSyntax.ExprDereference t v) =
  LinearSyntax.ExprDereference t v
linearizeExpr (SimplifiedSyntax.ExprAddressOf t v) =
  LinearSyntax.ExprAddressOf t v
linearizeExpr (SimplifiedSyntax.ExprConst t c) = LinearSyntax.ExprConst t c
linearizeExpr (SimplifiedSyntax.ExprBinOp op lhs rhs) =
  LinearSyntax.ExprBinOp op (linearizeExpr lhs) (linearizeExpr rhs)
linearizeExpr (SimplifiedSyntax.ExprUnOp op e) =
  LinearSyntax.ExprUnOp op (linearizeExpr e)
