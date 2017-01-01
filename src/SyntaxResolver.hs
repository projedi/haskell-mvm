module SyntaxResolver
  ( resolve
  ) where

import Control.Monad.State (State, evalState)
import qualified Control.Monad.State as State

import qualified PreSyntax
import qualified Syntax

resolve :: PreSyntax.Program -> Syntax.Program
resolve p =
  Syntax.Program
  { Syntax.programLibraries = PreSyntax.programLibraries p
  , Syntax.programStatements = code
  }
  where
    code = runResolver (resolveBlock $ PreSyntax.programStatements p)

data ResolverState = ResolverState
  { declaredVariables :: [Syntax.VarDecl]
  , localVariables :: [Syntax.VarDecl]
  }

emptyState :: ResolverState
emptyState = ResolverState
  { declaredVariables = []
  , localVariables = []
  }

type Resolver a = State ResolverState a

runResolver :: Resolver a -> a
runResolver m = evalState m emptyState

resolveBlock :: [PreSyntax.Statement] -> Resolver Syntax.Block
resolveBlock stmts = do
  stBefore <- State.get
  State.modify $ \st -> st
    { declaredVariables = localVariables st ++ declaredVariables st
    , localVariables = []
    }
  stmts' <- concat <$> mapM resolveStatement stmts
  vars <- (reverse . localVariables) <$> State.get
  State.modify $ \st -> st
    { declaredVariables = declaredVariables stBefore
    , localVariables = localVariables stBefore
    }
  pure $ Syntax.Block
    { Syntax.blockVariables = vars
    , Syntax.blockStatements = stmts'
    }

noopBlock :: Syntax.Block
noopBlock = Syntax.Block
  { Syntax.blockVariables = []
  , Syntax.blockStatements = []
  }

resolveStatement :: PreSyntax.Statement -> Resolver [Syntax.Statement]
resolveStatement (PreSyntax.StatementBlock stmts) =
  sequence [Syntax.StatementBlock <$> resolveBlock stmts]
resolveStatement (PreSyntax.StatementFunctionCall fcall) =
  sequence [Syntax.StatementFunctionCall <$> resolveFunctionCall fcall]
resolveStatement (PreSyntax.StatementWhile e stmt) =
  sequence
    [ Syntax.StatementWhile <$> resolveExpr e <*> resolveBlock [stmt]
    ]
resolveStatement (PreSyntax.StatementVarDecl vdecl) = do
  resolveVarDecl vdecl
  pure []
resolveStatement (PreSyntax.StatementVarDef vdecl@(PreSyntax.VarDecl _ vname) e) = do
  resolveVarDecl vdecl
  sequence
    [ Syntax.StatementAssign <$> resolveVarName vname <*> resolveExpr e
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
      (resolveBlock [s1]) <*>
      (resolveBlock [s2])
    ]
resolveStatement (PreSyntax.StatementIf e s) =
  sequence
    [ Syntax.StatementIfElse <$> resolveExpr e <*>
      (resolveBlock [s]) <*>
      pure noopBlock
    ]
resolveStatement (PreSyntax.StatementFor vname e1 e2 s) =
  sequence
    [ Syntax.StatementFor <$> resolveVarName vname <*> resolveExpr e1 <*>
      resolveExpr e2 <*> resolveBlock [s]
    ]
resolveStatement (PreSyntax.StatementFunctionDef fdecl stmts) =
  sequence
    [ Syntax.StatementFunctionDef <$> resolveFunctionDecl fdecl <*>
      resolveBlock stmts
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

resolveParam :: PreSyntax.VarDecl -> Resolver Syntax.VarDecl
resolveParam (PreSyntax.VarDecl vtype vname) =
  Syntax.VarDecl vtype <$> resolveVarName vname

resolveVarDecl :: PreSyntax.VarDecl -> Resolver ()
resolveVarDecl vdecl = do
  vdecl' <- resolveParam vdecl
  State.modify $ \st -> st { localVariables = vdecl' : localVariables st }

resolveFunctionDecl :: PreSyntax.FunctionDecl -> Resolver Syntax.FunctionDecl
resolveFunctionDecl (PreSyntax.FunctionDecl rettype name params) =
  Syntax.FunctionDecl rettype <$> resolveFunctionName name <*>
  mapM resolveParam params

resolveVarName :: PreSyntax.VarName -> Resolver Syntax.VarName
resolveVarName (PreSyntax.VarName name) = pure $ Syntax.VarName name

resolveFunctionName :: PreSyntax.FunctionName -> Resolver Syntax.FunctionName
resolveFunctionName (PreSyntax.FunctionName name) = pure $ Syntax.FunctionName name
