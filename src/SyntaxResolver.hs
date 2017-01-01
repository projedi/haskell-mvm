module SyntaxResolver
  ( resolve
  ) where

import Control.Monad.State (State, evalState)
import qualified Control.Monad.State as State
import Data.Foldable (asum)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map

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

data Layer = Layer
  { varEnv :: Map PreSyntax.VarName Syntax.VarID
  }

emptyLayer :: Layer
emptyLayer =
  Layer
  { varEnv = Map.empty
  }

findVariableInLayer :: Layer -> PreSyntax.VarName -> Maybe Syntax.VarID
findVariableInLayer Layer {varEnv = lvars} vname = Map.lookup vname lvars

newVariableInLayer :: Layer -> PreSyntax.VarName -> Syntax.VarID -> Maybe Layer
newVariableInLayer l@Layer {varEnv = lvars} vname vid =
  case Map.insertLookupWithKey (\_ val _ -> val) vname vid lvars of
    (Nothing, lvars') ->
      Just $
      l
      { varEnv = lvars'
      }
    (Just _, _) -> Nothing

data Env = Env
  { vars :: IntMap Syntax.VarType
  , lastVar :: Syntax.VarID
  , layers :: [Layer]
  }

emptyEnv :: Env
emptyEnv = Env
  { vars = IntMap.empty
  , lastVar = Syntax.VarID (-1)
  , layers = [emptyLayer]
  }

findVariableInEnv :: Env -> PreSyntax.VarName -> Syntax.VarID
findVariableInEnv env vname =
  let (Just v) = asum (map (`findVariableInLayer` vname) (layers env))
  in v

newVariableInEnv :: Env -> Syntax.VarType -> (Syntax.VarID, Env)
newVariableInEnv env vtype =
  let (Syntax.VarID v) = inc $ lastVar env
      (Nothing, vars') =
        IntMap.insertLookupWithKey (\_ val _ -> val) v vtype (vars env)
  in ( Syntax.VarID v
     , env
       { lastVar = Syntax.VarID v
       , vars = vars'
       })
  where
    inc (Syntax.VarID lastVarID) = Syntax.VarID (lastVarID + 1)

introduceVariableInEnv :: Env -> PreSyntax.VarDecl -> (Syntax.VarID, Env)
introduceVariableInEnv env (PreSyntax.VarDecl vtype vname) =
  let (v, env'@Env {layers = (l:ls)}) = newVariableInEnv env vtype
      Just l' = newVariableInLayer l vname v
  in ( v
     , env'
       { layers = l' : ls
       })

type Resolver a = State Env a

runResolver :: Resolver a -> a
runResolver m = evalState m emptyEnv

findVariable :: PreSyntax.VarName -> Resolver Syntax.VarID
findVariable vname = (`findVariableInEnv` vname) <$> State.get

introduceVariable :: PreSyntax.VarDecl -> Resolver Syntax.VarID
introduceVariable vdecl = do
  envBefore <- State.get
  let (v, envAfter) = introduceVariableInEnv envBefore vdecl
  State.put envAfter
  pure v

withLayer :: Resolver a -> Resolver a
withLayer m = do
  State.modify $ \env -> env { layers = emptyLayer : layers env }
  res <- m
  State.modify $ \env -> env { layers = tail $ layers env }
  pure res

resolveBlock :: [PreSyntax.Statement] -> Resolver Syntax.Block
resolveBlock stmts = withLayer $ do
  stmts' <- concat <$> mapM resolveStatement stmts
  l <- (head . layers) <$> State.get
  lvars <- variablesInLayer l
  pure $ Syntax.Block
    { Syntax.blockVariables = lvars
    , Syntax.blockStatements = stmts'
    }

variablesInLayer :: Layer -> Resolver [Syntax.VarDecl]
variablesInLayer l = do
  vs <- vars <$> State.get
  pure $ map (\vid -> go vs vid) $ Map.elems (varEnv l)
  where
    go vs (Syntax.VarID vid) =
      let Just vtype = IntMap.lookup vid vs
      in Syntax.VarDecl vtype (Syntax.VarID vid)

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
  _ <- introduceVariable vdecl
  pure []
resolveStatement (PreSyntax.StatementVarDef vdecl e) = do
  sequence
    [ Syntax.StatementAssign <$> introduceVariable vdecl <*> resolveExpr e
    ]
resolveStatement (PreSyntax.StatementFunctionDecl fdecl) =
  sequence [Syntax.StatementFunctionDecl <$> resolveFunctionDecl fdecl]
resolveStatement (PreSyntax.StatementAssign vname e) = do
  sequence [Syntax.StatementAssign <$> findVariable vname <*> resolveExpr e]
resolveStatement (PreSyntax.StatementAssignPlus vname e) =
  sequence [Syntax.StatementAssign <$> findVariable vname <*> resolveExpr e']
  where
    e' = PreSyntax.ExprPlus (PreSyntax.ExprVar vname) e
resolveStatement (PreSyntax.StatementAssignMinus vname e) =
  sequence [Syntax.StatementAssign <$> findVariable vname <*> resolveExpr e']
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
    [ Syntax.StatementFor <$> findVariable vname <*> resolveExpr e1 <*>
      resolveExpr e2 <*> resolveBlock [s]
    ]
resolveStatement (PreSyntax.StatementFunctionDef fdecl stmts) =
  sequence
    [ Syntax.StatementFunctionDef <$> resolveFunctionDef fdecl stmts
    ]
resolveStatement (PreSyntax.StatementReturn Nothing) =
  sequence [pure $ Syntax.StatementReturn Nothing]
resolveStatement (PreSyntax.StatementReturn (Just e)) =
  sequence [(Syntax.StatementReturn . Just) <$> resolveExpr e]
resolveStatement (PreSyntax.StatementForeignFunctionDecl fdecl) =
  sequence [Syntax.StatementForeignFunctionDecl <$> resolveFunctionDecl fdecl]

resolveFunctionDef :: PreSyntax.FunctionDecl -> [PreSyntax.Statement] -> Resolver Syntax.FunctionDef
resolveFunctionDef fdecl@(PreSyntax.FunctionDecl _ _ params) stmts = do
  fdecl' <- resolveFunctionDecl fdecl
  withLayer $ do
    params' <- mapM introduceVariable params
    body <- resolveBlock stmts
    pure $ Syntax.FunctionDef fdecl' params' body

resolveFunctionCall :: PreSyntax.FunctionCall -> Resolver Syntax.FunctionCall
resolveFunctionCall (PreSyntax.FunctionCall fname args) =
  Syntax.FunctionCall <$> resolveFunctionName fname <*> mapM resolveExpr args

resolveExpr :: PreSyntax.Expr -> Resolver Syntax.Expr
resolveExpr (PreSyntax.ExprFunctionCall fcall) =
  Syntax.ExprFunctionCall <$> resolveFunctionCall fcall
resolveExpr (PreSyntax.ExprVar vname) = Syntax.ExprVar <$> findVariable vname
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

resolveFunctionDecl :: PreSyntax.FunctionDecl -> Resolver Syntax.FunctionDecl
resolveFunctionDecl (PreSyntax.FunctionDecl rettype name params) =
  Syntax.FunctionDecl rettype <$> resolveFunctionName name <*> pure (map (\(PreSyntax.VarDecl vtype _) -> vtype) params)

resolveFunctionName :: PreSyntax.FunctionName -> Resolver Syntax.FunctionName
resolveFunctionName (PreSyntax.FunctionName name) = pure $ Syntax.FunctionName name
