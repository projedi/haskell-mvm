module TypeChecker
  ( typeCheck
  ) where

import Control.Monad.State (State, runState)
import Data.IntMap (IntMap)

import qualified ResolvedSyntax
import qualified Syntax

typeCheck :: ResolvedSyntax.Program -> Syntax.Program
typeCheck p =
  Syntax.Program
  { Syntax.programFunctions = fs
  , Syntax.programLibraries = ResolvedSyntax.programLibraries p
  , Syntax.programForeignFunctions = envForeignFunctions finalEnv
  , Syntax.programLastFunID = envLastFunID finalEnv
  , Syntax.programLastVarID = envLastVarID finalEnv
  }
  where
    startEnv =
      Env
      { envFuns = ResolvedSyntax.programFunctions p
      , envForeignFunctions = ResolvedSyntax.programForeignFunctions p
      , envLastFunID = ResolvedSyntax.programLastFunID p
      , envLastVarID = ResolvedSyntax.programLastVarID p
      }
    (fs, finalEnv) = runTypeChecker (ResolvedSyntax.programFunctions p) startEnv

data Env = Env
  { envFuns :: IntMap ResolvedSyntax.FunctionDef
  , envForeignFunctions :: IntMap ResolvedSyntax.ForeignFunctionDecl
  , envLastFunID :: Syntax.FunID
  , envLastVarID :: Syntax.VarID
  }

type TypeChecker = State Env

runTypeChecker :: IntMap ResolvedSyntax.FunctionDef
               -> Env
               -> (IntMap Syntax.FunctionDef, Env)
runTypeChecker fs = runState (sequence $ fmap typecheckFunDef fs)

typecheckFunDef :: ResolvedSyntax.FunctionDef -> TypeChecker Syntax.FunctionDef
typecheckFunDef f = do
  body <- typecheckBlock $ ResolvedSyntax.funDefBody f
  pure
    Syntax.FunctionDef
    { Syntax.funDefRetType = ResolvedSyntax.funDefRetType f
    , Syntax.funDefName = ResolvedSyntax.funDefName f
    , Syntax.funDefParams = ResolvedSyntax.funDefParams f
    , Syntax.funDefAccesses = ResolvedSyntax.funDefAccesses f
    , Syntax.funDefBody = body
    }

typecheckBlock :: ResolvedSyntax.Block -> TypeChecker Syntax.Block
typecheckBlock block = do
  stmts <- mapM typecheckStatement (ResolvedSyntax.blockStatements block)
  pure
    Syntax.Block
    { Syntax.blockVariables = ResolvedSyntax.blockVariables block
    , Syntax.blockStatements = stmts
    }

noopBlock :: Syntax.Block
noopBlock =
  Syntax.Block
  { Syntax.blockVariables = []
  , Syntax.blockStatements = []
  }

typecheckStatement :: ResolvedSyntax.Statement -> TypeChecker Syntax.Statement
typecheckStatement (ResolvedSyntax.StatementBlock block) =
  Syntax.StatementBlock <$> typecheckBlock block
typecheckStatement (ResolvedSyntax.StatementFunctionCall fcall) =
  Syntax.StatementFunctionCall <$> typecheckFunctionCall fcall
typecheckStatement (ResolvedSyntax.StatementWhile e block) =
  Syntax.StatementWhile <$> typecheckExpr e <*> typecheckBlock block
typecheckStatement (ResolvedSyntax.StatementAssign v e) =
  Syntax.StatementAssign v <$> typecheckExpr e
typecheckStatement (ResolvedSyntax.StatementAssignPlus v e) =
  Syntax.StatementAssign v <$> typecheckExpr e'
  where
    e' = ResolvedSyntax.ExprPlus (ResolvedSyntax.ExprVar v) e
typecheckStatement (ResolvedSyntax.StatementAssignMinus v e) =
  Syntax.StatementAssign v <$> typecheckExpr e'
  where
    e' = ResolvedSyntax.ExprMinus (ResolvedSyntax.ExprVar v) e
typecheckStatement (ResolvedSyntax.StatementIfElse e bt bf) =
  Syntax.StatementIfElse <$> typecheckExpr e <*> typecheckBlock bt <*>
  typecheckBlock bf
typecheckStatement (ResolvedSyntax.StatementIf e bt) =
  Syntax.StatementIfElse <$> typecheckExpr e <*> typecheckBlock bt <*> pure noopBlock
typecheckStatement (ResolvedSyntax.StatementReturn Nothing) =
  pure $ Syntax.StatementReturn Nothing
typecheckStatement (ResolvedSyntax.StatementReturn (Just e)) =
  (Syntax.StatementReturn . Just) <$> typecheckExpr e

typecheckFunctionCall :: ResolvedSyntax.FunctionCall -> TypeChecker Syntax.FunctionCall
typecheckFunctionCall (ResolvedSyntax.NativeFunctionCall f args) =
  Syntax.NativeFunctionCall f <$> mapM typecheckExpr args
typecheckFunctionCall (ResolvedSyntax.ForeignFunctionCall f args) =
  Syntax.ForeignFunctionCall f <$> mapM typecheckExpr args
typecheckFunctionCall (ResolvedSyntax.PrintCall args) =
  Syntax.PrintCall <$> mapM typecheckExpr args

typecheckExpr :: ResolvedSyntax.Expr -> TypeChecker Syntax.Expr
typecheckExpr (ResolvedSyntax.ExprFunctionCall fcall) =
  Syntax.ExprFunctionCall <$> typecheckFunctionCall fcall
typecheckExpr (ResolvedSyntax.ExprVar v) = pure $ Syntax.ExprVar v
typecheckExpr (ResolvedSyntax.ExprInt i) = pure $ Syntax.ExprInt i
typecheckExpr (ResolvedSyntax.ExprFloat f) = pure $ Syntax.ExprFloat f
typecheckExpr (ResolvedSyntax.ExprString s) = pure $ Syntax.ExprString s
typecheckExpr (ResolvedSyntax.ExprNeg e) = Syntax.ExprNeg <$> typecheckExpr e
typecheckExpr (ResolvedSyntax.ExprPlus lhs rhs) =
  Syntax.ExprPlus <$> typecheckExpr lhs <*> typecheckExpr rhs
typecheckExpr (ResolvedSyntax.ExprMinus lhs rhs) =
  Syntax.ExprMinus <$> typecheckExpr lhs <*> typecheckExpr rhs
typecheckExpr (ResolvedSyntax.ExprTimes lhs rhs) =
  Syntax.ExprTimes <$> typecheckExpr lhs <*> typecheckExpr rhs
typecheckExpr (ResolvedSyntax.ExprDiv lhs rhs) =
  Syntax.ExprDiv <$> typecheckExpr lhs <*> typecheckExpr rhs
typecheckExpr (ResolvedSyntax.ExprMod lhs rhs) =
  Syntax.ExprMod <$> typecheckExpr lhs <*> typecheckExpr rhs
typecheckExpr (ResolvedSyntax.ExprBitAnd lhs rhs) =
  Syntax.ExprBitAnd <$> typecheckExpr lhs <*> typecheckExpr rhs
typecheckExpr (ResolvedSyntax.ExprBitOr lhs rhs) =
  Syntax.ExprBitOr <$> typecheckExpr lhs <*> typecheckExpr rhs
typecheckExpr (ResolvedSyntax.ExprBitXor lhs rhs) =
  Syntax.ExprBitXor <$> typecheckExpr lhs <*> typecheckExpr rhs
typecheckExpr (ResolvedSyntax.ExprNot e) = Syntax.ExprNot <$> typecheckExpr e
typecheckExpr (ResolvedSyntax.ExprAnd lhs rhs) =
  Syntax.ExprAnd <$> typecheckExpr lhs <*> typecheckExpr rhs
typecheckExpr (ResolvedSyntax.ExprOr lhs rhs) =
  Syntax.ExprOr <$> typecheckExpr lhs <*> typecheckExpr rhs
typecheckExpr (ResolvedSyntax.ExprEq lhs rhs) =
  Syntax.ExprEq <$> typecheckExpr lhs <*> typecheckExpr rhs
typecheckExpr (ResolvedSyntax.ExprNeq lhs rhs) =
  Syntax.ExprNot <$> (Syntax.ExprEq <$> typecheckExpr lhs <*> typecheckExpr rhs)
typecheckExpr (ResolvedSyntax.ExprLt lhs rhs) =
  Syntax.ExprLt <$> typecheckExpr lhs <*> typecheckExpr rhs
typecheckExpr (ResolvedSyntax.ExprLeq lhs rhs) =
  Syntax.ExprNot <$> (Syntax.ExprLt <$> typecheckExpr rhs <*> typecheckExpr lhs)
typecheckExpr (ResolvedSyntax.ExprGt lhs rhs) =
  Syntax.ExprLt <$> typecheckExpr rhs <*> typecheckExpr lhs
typecheckExpr (ResolvedSyntax.ExprGeq lhs rhs) =
  Syntax.ExprNot <$> (Syntax.ExprLt <$> typecheckExpr lhs <*> typecheckExpr rhs)
