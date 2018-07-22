module SyntaxSimplifier
  ( simplify
  ) where

import qualified Data.IntMap as IntMap

import qualified SimplifiedSyntax
import qualified TypedSyntax

simplify :: TypedSyntax.Program -> SimplifiedSyntax.Program
simplify p =
  SimplifiedSyntax.Program
    { SimplifiedSyntax.programFunctions =
        IntMap.map simplifyFunctionDef $ TypedSyntax.programFunctions p
    , SimplifiedSyntax.programLibraries = TypedSyntax.programLibraries p
    , SimplifiedSyntax.programForeignFunctions =
        TypedSyntax.programForeignFunctions p
    , SimplifiedSyntax.programConstants = TypedSyntax.programConstants p
    , SimplifiedSyntax.programVariables = TypedSyntax.programVariables p
    }

simplifyFunctionDef :: TypedSyntax.FunctionDef -> SimplifiedSyntax.FunctionDef
simplifyFunctionDef fdef =
  SimplifiedSyntax.FunctionDef
    { SimplifiedSyntax.funDefRetType = TypedSyntax.funDefRetType fdef
    , SimplifiedSyntax.funDefName = TypedSyntax.funDefName fdef
    , SimplifiedSyntax.funDefParams = TypedSyntax.funDefParams fdef
    , SimplifiedSyntax.funDefCaptures = TypedSyntax.funDefCaptures fdef
    , SimplifiedSyntax.funDefBody = simplifyBlock $ TypedSyntax.funDefBody fdef
    }

simplifyBlock :: TypedSyntax.Block -> SimplifiedSyntax.Block
simplifyBlock block =
  SimplifiedSyntax.Block
    { SimplifiedSyntax.blockStatements =
        map simplifyStatement $ TypedSyntax.blockStatements block
    }

simplifyStatement :: TypedSyntax.Statement -> SimplifiedSyntax.Statement
simplifyStatement (TypedSyntax.StatementBlock block) =
  SimplifiedSyntax.StatementBlock $ simplifyBlock block
simplifyStatement (TypedSyntax.StatementVarAlloc v) =
  SimplifiedSyntax.StatementVarAlloc v
simplifyStatement (TypedSyntax.StatementFunctionCall fcall) =
  SimplifiedSyntax.StatementFunctionCall $ simplifyFunctionCall fcall
simplifyStatement (TypedSyntax.StatementWhile expr block) =
  SimplifiedSyntax.StatementWhile (simplifyExpr expr) (simplifyBlock block)
simplifyStatement (TypedSyntax.StatementAssign v expr) =
  SimplifiedSyntax.StatementAssign v (simplifyExpr expr)
simplifyStatement (TypedSyntax.StatementIfElse expr blockTrue blockFalse) =
  SimplifiedSyntax.StatementIfElse
    (simplifyExpr expr)
    (simplifyBlock blockTrue)
    (simplifyBlock blockFalse)
simplifyStatement (TypedSyntax.StatementReturn mExpr) =
  SimplifiedSyntax.StatementReturn (simplifyExpr <$> mExpr)

simplifyFunctionCall ::
     TypedSyntax.FunctionCall -> SimplifiedSyntax.FunctionCall
simplifyFunctionCall fcall@TypedSyntax.NativeFunctionCall {} =
  SimplifiedSyntax.NativeFunctionCall
    { SimplifiedSyntax.nativeFunCallName = TypedSyntax.nativeFunCallName fcall
    , SimplifiedSyntax.nativeFunCallRetType =
        TypedSyntax.nativeFunCallRetType fcall
    , SimplifiedSyntax.nativeFunCallArgs =
        map simplifyExpr $ TypedSyntax.nativeFunCallArgs fcall
    }
simplifyFunctionCall fcall@TypedSyntax.ForeignFunctionCall {} =
  SimplifiedSyntax.ForeignFunctionCall
    { SimplifiedSyntax.foreignFunCallName = TypedSyntax.foreignFunCallName fcall
    , SimplifiedSyntax.foreignFunCallRetType =
        TypedSyntax.foreignFunCallRetType fcall
    , SimplifiedSyntax.foreignFunCallArgs =
        map simplifyExpr $ TypedSyntax.foreignFunCallArgs fcall
    }
simplifyFunctionCall (TypedSyntax.PrintCall es) =
  SimplifiedSyntax.PrintCall $ map simplifyExpr es

simplifyExpr :: TypedSyntax.Expr -> SimplifiedSyntax.Expr
simplifyExpr (TypedSyntax.ExprFunctionCall fcall) =
  SimplifiedSyntax.ExprFunctionCall $ simplifyFunctionCall fcall
simplifyExpr (TypedSyntax.ExprVar t v) = SimplifiedSyntax.ExprVar t v
simplifyExpr (TypedSyntax.ExprConst t c) = SimplifiedSyntax.ExprConst t c
simplifyExpr (TypedSyntax.ExprBinOp op lhs rhs) =
  simplifyBinOp op (simplifyExpr lhs) (simplifyExpr rhs)
simplifyExpr (TypedSyntax.ExprUnOp op arg) = simplifyUnOp op (simplifyExpr arg)

simplifyBinOp ::
     TypedSyntax.BinOp
  -> SimplifiedSyntax.Expr
  -> SimplifiedSyntax.Expr
  -> SimplifiedSyntax.Expr
simplifyBinOp TypedSyntax.BinPlus =
  SimplifiedSyntax.ExprBinOp SimplifiedSyntax.BinPlus
simplifyBinOp TypedSyntax.BinMinus =
  SimplifiedSyntax.ExprBinOp SimplifiedSyntax.BinMinus
simplifyBinOp TypedSyntax.BinTimes =
  SimplifiedSyntax.ExprBinOp SimplifiedSyntax.BinTimes
simplifyBinOp TypedSyntax.BinDiv =
  SimplifiedSyntax.ExprBinOp SimplifiedSyntax.BinDiv
simplifyBinOp TypedSyntax.BinMod =
  SimplifiedSyntax.ExprBinOp SimplifiedSyntax.BinMod
simplifyBinOp TypedSyntax.BinBitAnd =
  SimplifiedSyntax.ExprBinOp SimplifiedSyntax.BinBitAnd
simplifyBinOp TypedSyntax.BinBitOr =
  SimplifiedSyntax.ExprBinOp SimplifiedSyntax.BinBitOr
simplifyBinOp TypedSyntax.BinBitXor =
  SimplifiedSyntax.ExprBinOp SimplifiedSyntax.BinBitXor
simplifyBinOp TypedSyntax.BinAnd =
  SimplifiedSyntax.ExprBinOp SimplifiedSyntax.BinAnd
simplifyBinOp TypedSyntax.BinOr =
  SimplifiedSyntax.ExprBinOp SimplifiedSyntax.BinOr
simplifyBinOp TypedSyntax.BinEq =
  SimplifiedSyntax.ExprBinOp SimplifiedSyntax.BinEq
simplifyBinOp TypedSyntax.BinLt =
  SimplifiedSyntax.ExprBinOp SimplifiedSyntax.BinLt

simplifyUnOp ::
     TypedSyntax.UnOp -> SimplifiedSyntax.Expr -> SimplifiedSyntax.Expr
simplifyUnOp TypedSyntax.UnNeg =
  SimplifiedSyntax.ExprUnOp SimplifiedSyntax.UnNeg
simplifyUnOp TypedSyntax.UnNot =
  SimplifiedSyntax.ExprUnOp SimplifiedSyntax.UnNot
simplifyUnOp TypedSyntax.UnIntToFloat =
  SimplifiedSyntax.ExprUnOp SimplifiedSyntax.UnIntToFloat
