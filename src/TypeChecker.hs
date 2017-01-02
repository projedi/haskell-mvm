module TypeChecker
  ( typeCheck
  ) where

import Data.IntMap (IntMap)

import qualified ResolvedSyntax
import qualified Syntax

typeCheck :: ResolvedSyntax.Program -> Syntax.Program
typeCheck p =
  Syntax.Program
  { Syntax.programFunctions = runTypeChecker $ ResolvedSyntax.programFunctions p
  , Syntax.programLibraries = ResolvedSyntax.programLibraries p
  , Syntax.programForeignFunctions = ResolvedSyntax.programForeignFunctions p
  }

runTypeChecker :: IntMap ResolvedSyntax.FunctionDef -> IntMap Syntax.FunctionDef
runTypeChecker = fmap typecheckFunDef

typecheckFunDef :: ResolvedSyntax.FunctionDef -> Syntax.FunctionDef
typecheckFunDef f =
  Syntax.FunctionDef
  { Syntax.funDefRetType = ResolvedSyntax.funDefRetType f
  , Syntax.funDefName = ResolvedSyntax.funDefName f
  , Syntax.funDefParams = ResolvedSyntax.funDefParams f
  , Syntax.funDefAccesses = ResolvedSyntax.funDefAccesses f
  , Syntax.funDefBody = typecheckBlock $ ResolvedSyntax.funDefBody f
  }

typecheckBlock :: ResolvedSyntax.Block -> Syntax.Block
typecheckBlock block =
  Syntax.Block
  { Syntax.blockVariables = ResolvedSyntax.blockVariables block
  , Syntax.blockStatements =
    map typecheckStatement $ ResolvedSyntax.blockStatements block
  }

typecheckStatement :: ResolvedSyntax.Statement -> Syntax.Statement
typecheckStatement ResolvedSyntax.StatementNoop = Syntax.StatementNoop
typecheckStatement (ResolvedSyntax.StatementBlock block) =
  Syntax.StatementBlock $ typecheckBlock block
typecheckStatement (ResolvedSyntax.StatementFunctionCall fcall) =
  Syntax.StatementFunctionCall $ typecheckFunctionCall fcall
typecheckStatement (ResolvedSyntax.StatementWhile e block) =
  Syntax.StatementWhile (typecheckExpr e) (typecheckBlock block)
typecheckStatement (ResolvedSyntax.StatementAssign v e) =
  Syntax.StatementAssign v (typecheckExpr e)
typecheckStatement (ResolvedSyntax.StatementIfElse e bt bf) =
  Syntax.StatementIfElse
    (typecheckExpr e)
    (typecheckBlock bt)
    (typecheckBlock bf)
typecheckStatement (ResolvedSyntax.StatementReturn Nothing) =
  Syntax.StatementReturn Nothing
typecheckStatement (ResolvedSyntax.StatementReturn (Just e)) =
  Syntax.StatementReturn (Just (typecheckExpr e))

typecheckFunctionCall :: ResolvedSyntax.FunctionCall -> Syntax.FunctionCall
typecheckFunctionCall (ResolvedSyntax.NativeFunctionCall f args) =
  Syntax.NativeFunctionCall f (map typecheckExpr args)
typecheckFunctionCall (ResolvedSyntax.ForeignFunctionCall f args) =
  Syntax.ForeignFunctionCall f (map typecheckExpr args)
typecheckFunctionCall (ResolvedSyntax.PrintCall args) =
  Syntax.PrintCall (map typecheckExpr args)

typecheckExpr :: ResolvedSyntax.Expr -> Syntax.Expr
typecheckExpr (ResolvedSyntax.ExprFunctionCall fcall) =
  Syntax.ExprFunctionCall (typecheckFunctionCall fcall)
typecheckExpr (ResolvedSyntax.ExprVar v) = Syntax.ExprVar v
typecheckExpr (ResolvedSyntax.ExprInt i) = Syntax.ExprInt i
typecheckExpr (ResolvedSyntax.ExprFloat f) = Syntax.ExprFloat f
typecheckExpr (ResolvedSyntax.ExprString s) = Syntax.ExprString s
typecheckExpr (ResolvedSyntax.ExprNeg e) = Syntax.ExprNeg $ typecheckExpr e
typecheckExpr (ResolvedSyntax.ExprPlus lhs rhs) =
  Syntax.ExprPlus (typecheckExpr lhs) (typecheckExpr rhs)
typecheckExpr (ResolvedSyntax.ExprMinus lhs rhs) =
  Syntax.ExprMinus (typecheckExpr lhs) (typecheckExpr rhs)
typecheckExpr (ResolvedSyntax.ExprTimes lhs rhs) =
  Syntax.ExprTimes (typecheckExpr lhs) (typecheckExpr rhs)
typecheckExpr (ResolvedSyntax.ExprDiv lhs rhs) =
  Syntax.ExprDiv (typecheckExpr lhs) (typecheckExpr rhs)
typecheckExpr (ResolvedSyntax.ExprMod lhs rhs) =
  Syntax.ExprMod (typecheckExpr lhs) (typecheckExpr rhs)
typecheckExpr (ResolvedSyntax.ExprBitAnd lhs rhs) =
  Syntax.ExprBitAnd (typecheckExpr lhs) (typecheckExpr rhs)
typecheckExpr (ResolvedSyntax.ExprBitOr lhs rhs) =
  Syntax.ExprBitOr (typecheckExpr lhs) (typecheckExpr rhs)
typecheckExpr (ResolvedSyntax.ExprBitXor lhs rhs) =
  Syntax.ExprBitXor (typecheckExpr lhs) (typecheckExpr rhs)
typecheckExpr (ResolvedSyntax.ExprNot e) = Syntax.ExprNot (typecheckExpr e)
typecheckExpr (ResolvedSyntax.ExprAnd lhs rhs) =
  Syntax.ExprAnd (typecheckExpr lhs) (typecheckExpr rhs)
typecheckExpr (ResolvedSyntax.ExprOr lhs rhs) =
  Syntax.ExprOr (typecheckExpr lhs) (typecheckExpr rhs)
typecheckExpr (ResolvedSyntax.ExprEq lhs rhs) =
  Syntax.ExprEq (typecheckExpr lhs) (typecheckExpr rhs)
typecheckExpr (ResolvedSyntax.ExprLt lhs rhs) =
  Syntax.ExprLt (typecheckExpr lhs) (typecheckExpr rhs)
