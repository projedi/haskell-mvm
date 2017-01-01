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
resolveStatements stmts = concat <$> mapM resolveStatement stmts

resolveStatement :: PreSyntax.Statement -> Resolver [Syntax.Statement]
resolveStatement (PreSyntax.StatementBlock stmts) =
  sequence [Syntax.StatementBlock <$> resolveStatements stmts]
resolveStatement (PreSyntax.StatementFunctionCall fcall) =
  sequence [Syntax.StatementFunctionCall <$> resolveFunctionCall fcall]
resolveStatement (PreSyntax.StatementWhile e stmt) =
  sequence
    [ Syntax.StatementWhile <$> resolveExpr e <*>
      (Syntax.StatementBlock <$> resolveStatement stmt)
    ]
resolveStatement (PreSyntax.StatementVarDecl vdecl) =
  sequence [Syntax.StatementVarDecl <$> resolveVarDecl vdecl]
resolveStatement (PreSyntax.StatementVarDef vdecl@(PreSyntax.VarDecl _ vname) e) =
  sequence
    [ Syntax.StatementVarDecl <$> resolveVarDecl vdecl
    , Syntax.StatementAssign <$> resolveVarName vname <*> resolveExpr e
    ]
resolveStatement (PreSyntax.StatementFunctionDecl fdecl) =
  sequence [Syntax.StatementFunctionDecl <$> resolveFunctionDecl fdecl]
resolveStatement (PreSyntax.StatementAssign vname e) =
  sequence [Syntax.StatementAssign <$> resolveVarName vname <*> resolveExpr e]
resolveStatement (PreSyntax.StatementAssignPlus vname e) =
  sequence [Syntax.StatementAssign <$> resolveVarName vname <*> resolveExpr e']
  where
    e' = PreSyntax.ExprPlus (PreSyntax.ExprVar vname) e
resolveStatement (PreSyntax.StatementAssignMinus vname e) =
  sequence [Syntax.StatementAssign <$> resolveVarName vname <*> resolveExpr e']
  where
    e' = PreSyntax.ExprMinus (PreSyntax.ExprVar vname) e
resolveStatement (PreSyntax.StatementIfElse e s1 s2) =
  sequence
    [ Syntax.StatementIfElse <$> resolveExpr e <*>
      (Syntax.StatementBlock <$> resolveStatement s1) <*>
      (Syntax.StatementBlock <$> resolveStatement s2)
    ]
resolveStatement (PreSyntax.StatementIf e s) =
  sequence
    [ Syntax.StatementIfElse <$> resolveExpr e <*>
      (Syntax.StatementBlock <$> resolveStatement s) <*>
      pure Syntax.StatementNoop
    ]
resolveStatement (PreSyntax.StatementFor vname e1 e2 s) =
  sequence
    [ Syntax.StatementFor <$> resolveVarName vname <*> resolveExpr e1 <*>
      resolveExpr e2 <*>
      (Syntax.StatementBlock <$> resolveStatement s)
    ]
resolveStatement (PreSyntax.StatementFunctionDef fdecl stmts) =
  sequence
    [ Syntax.StatementFunctionDef <$> resolveFunctionDecl fdecl <*>
      resolveStatements stmts
    ]
resolveStatement (PreSyntax.StatementReturn Nothing) =
  sequence [pure $ Syntax.StatementReturn Nothing]
resolveStatement (PreSyntax.StatementReturn (Just e)) =
  sequence [(Syntax.StatementReturn . Just) <$> resolveExpr e]
resolveStatement (PreSyntax.StatementForeignFunctionDecl fdecl) =
  sequence [Syntax.StatementForeignFunctionDecl <$> resolveFunctionDecl fdecl]

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
  resolveExpr $ PreSyntax.ExprNot (PreSyntax.ExprEq lhs rhs)
resolveExpr (PreSyntax.ExprLt lhs rhs) =
  Syntax.ExprLt <$> resolveExpr lhs <*> resolveExpr rhs
resolveExpr (PreSyntax.ExprLeq lhs rhs) =
  resolveExpr $ PreSyntax.ExprNot (PreSyntax.ExprGt lhs rhs)
resolveExpr (PreSyntax.ExprGt lhs rhs) = resolveExpr $ PreSyntax.ExprLt rhs lhs
resolveExpr (PreSyntax.ExprGeq lhs rhs) =
  resolveExpr $ PreSyntax.ExprNot (PreSyntax.ExprLt lhs rhs)

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
