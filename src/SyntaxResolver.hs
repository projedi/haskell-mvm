module SyntaxResolver
  ( resolve
  ) where

import Data.Functor.Identity (Identity, runIdentity)

import qualified PreSyntax
import qualified Syntax

resolve :: PreSyntax.Program -> Syntax.Program
resolve p =
  Syntax.Program
  { Syntax.programLibraries = PreSyntax.programLibraries p
  , Syntax.programStatements = code
  }
  where
    code = runResolver (resolveStatements $ PreSyntax.programStatements p)

type Resolver a = Identity a

runResolver :: Resolver a -> a
runResolver = runIdentity

resolveStatements :: [PreSyntax.Statement] -> Resolver [Syntax.Statement]
resolveStatements = mapM resolveStatement

resolveStatement :: PreSyntax.Statement -> Resolver Syntax.Statement
resolveStatement (PreSyntax.StatementBlock stmts) =
  Syntax.StatementBlock <$> resolveStatements stmts
resolveStatement (PreSyntax.StatementFunctionCall fcall) =
  Syntax.StatementFunctionCall <$> resolveFunctionCall fcall
resolveStatement (PreSyntax.StatementWhile e stmt) =
  Syntax.StatementWhile <$> resolveExpr e <*> resolveStatement stmt
resolveStatement (PreSyntax.StatementVarDecl vdecl) =
  Syntax.StatementVarDecl <$> resolveVarDecl vdecl
resolveStatement (PreSyntax.StatementVarDef vdecl e) =
  Syntax.StatementVarDef <$> resolveVarDecl vdecl <*> resolveExpr e
resolveStatement (PreSyntax.StatementFunctionDecl fdecl) =
  Syntax.StatementFunctionDecl <$> resolveFunctionDecl fdecl
resolveStatement (PreSyntax.StatementAssign vname e) =
  Syntax.StatementAssign <$> resolveVarName vname <*> resolveExpr e
resolveStatement (PreSyntax.StatementAssignPlus vname e) =
  Syntax.StatementAssignPlus <$> resolveVarName vname <*> resolveExpr e
resolveStatement (PreSyntax.StatementAssignMinus vname e) =
  Syntax.StatementAssignMinus <$> resolveVarName vname <*> resolveExpr e
resolveStatement (PreSyntax.StatementIfElse e s1 s2) =
  Syntax.StatementIfElse <$> resolveExpr e <*> resolveStatement s1 <*>
  resolveStatement s2
resolveStatement (PreSyntax.StatementIf e s) =
  Syntax.StatementIf <$> resolveExpr e <*> resolveStatement s
resolveStatement (PreSyntax.StatementFor vname e1 e2 s) =
  Syntax.StatementFor <$> resolveVarName vname <*> resolveExpr e1 <*>
  resolveExpr e2 <*>
  resolveStatement s
resolveStatement (PreSyntax.StatementFunctionDef fdecl stmts) =
  Syntax.StatementFunctionDef <$> resolveFunctionDecl fdecl <*>
  resolveStatements stmts
resolveStatement (PreSyntax.StatementReturn Nothing) =
  pure $ Syntax.StatementReturn Nothing
resolveStatement (PreSyntax.StatementReturn (Just e)) =
  (Syntax.StatementReturn . Just) <$> resolveExpr e
resolveStatement (PreSyntax.StatementForeignFunctionDecl fdecl) =
  Syntax.StatementForeignFunctionDecl <$> resolveFunctionDecl fdecl

resolveFunctionCall :: PreSyntax.FunctionCall -> Resolver Syntax.FunctionCall
resolveFunctionCall (PreSyntax.FunctionCall fname args) =
  Syntax.FunctionCall <$> resolveFunctionName fname <*> mapM resolveExpr args

resolveExpr :: PreSyntax.Expr -> Resolver Syntax.Expr
resolveExpr (PreSyntax.ExprFunctionCall fcall) =
  Syntax.ExprFunctionCall <$> resolveFunctionCall fcall
resolveExpr (PreSyntax.ExprVar vname) = Syntax.ExprVar <$> resolveVarName vname
resolveExpr (PreSyntax.ExprInt i) = pure $ Syntax.ExprInt i
resolveExpr (PreSyntax.ExprFloat f) = pure $ Syntax.ExprFloat f
resolveExpr (PreSyntax.ExprString s) = pure $ Syntax.ExprString s
resolveExpr (PreSyntax.ExprNeg e) = Syntax.ExprNeg <$> resolveExpr e
resolveExpr (PreSyntax.ExprPlus lhs rhs) =
  Syntax.ExprPlus <$> resolveExpr lhs <*> resolveExpr rhs
resolveExpr (PreSyntax.ExprMinus lhs rhs) =
  Syntax.ExprMinus <$> resolveExpr lhs <*> resolveExpr rhs
resolveExpr (PreSyntax.ExprTimes lhs rhs) =
  Syntax.ExprTimes <$> resolveExpr lhs <*> resolveExpr rhs
resolveExpr (PreSyntax.ExprDiv lhs rhs) =
  Syntax.ExprDiv <$> resolveExpr lhs <*> resolveExpr rhs
resolveExpr (PreSyntax.ExprMod lhs rhs) =
  Syntax.ExprMod <$> resolveExpr lhs <*> resolveExpr rhs
resolveExpr (PreSyntax.ExprBitAnd lhs rhs) =
  Syntax.ExprBitAnd <$> resolveExpr lhs <*> resolveExpr rhs
resolveExpr (PreSyntax.ExprBitOr lhs rhs) =
  Syntax.ExprBitOr <$> resolveExpr lhs <*> resolveExpr rhs
resolveExpr (PreSyntax.ExprBitXor lhs rhs) =
  Syntax.ExprBitXor <$> resolveExpr lhs <*> resolveExpr rhs
resolveExpr (PreSyntax.ExprNot e) = Syntax.ExprNot <$> resolveExpr e
resolveExpr (PreSyntax.ExprAnd lhs rhs) =
  Syntax.ExprAnd <$> resolveExpr lhs <*> resolveExpr rhs
resolveExpr (PreSyntax.ExprOr lhs rhs) =
  Syntax.ExprOr <$> resolveExpr lhs <*> resolveExpr rhs
resolveExpr (PreSyntax.ExprEq lhs rhs) =
  Syntax.ExprEq <$> resolveExpr lhs <*> resolveExpr rhs
resolveExpr (PreSyntax.ExprNeq lhs rhs) =
  Syntax.ExprNeq <$> resolveExpr lhs <*> resolveExpr rhs
resolveExpr (PreSyntax.ExprLt lhs rhs) =
  Syntax.ExprLt <$> resolveExpr lhs <*> resolveExpr rhs
resolveExpr (PreSyntax.ExprLeq lhs rhs) =
  Syntax.ExprLeq <$> resolveExpr lhs <*> resolveExpr rhs
resolveExpr (PreSyntax.ExprGt lhs rhs) =
  Syntax.ExprGt <$> resolveExpr lhs <*> resolveExpr rhs
resolveExpr (PreSyntax.ExprGeq lhs rhs) =
  Syntax.ExprGeq <$> resolveExpr lhs <*> resolveExpr rhs

resolveVarDecl :: PreSyntax.VarDecl -> Resolver Syntax.VarDecl
resolveVarDecl (PreSyntax.VarDecl vtype vname) =
  Syntax.VarDecl vtype <$> resolveVarName vname

resolveFunctionDecl :: PreSyntax.FunctionDecl -> Resolver Syntax.FunctionDecl
resolveFunctionDecl (PreSyntax.FunctionDecl rettype name params) =
  Syntax.FunctionDecl rettype <$> resolveFunctionName name <*>
  mapM resolveVarDecl params

resolveVarName :: PreSyntax.VarName -> Resolver Syntax.VarName
resolveVarName (PreSyntax.VarName name) = pure $ Syntax.VarName name

resolveFunctionName :: PreSyntax.FunctionName -> Resolver Syntax.FunctionName
resolveFunctionName (PreSyntax.FunctionName name) = pure $ Syntax.FunctionName name
