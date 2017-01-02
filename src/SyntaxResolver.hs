module SyntaxResolver
  ( resolve
  ) where

import Control.Monad
import Control.Monad.State (State, execState)
import qualified Control.Monad.State as State
import Control.Monad.Writer (WriterT, runWriterT)
import qualified Control.Monad.Writer as Writer
import Data.Foldable (asum)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import Data.Map (Map)
import qualified Data.Map as Map

import qualified PreSyntax
import qualified Syntax

resolve :: PreSyntax.Program -> Syntax.Program
resolve p =
  Syntax.Program
  { Syntax.programLibraries = PreSyntax.programLibraries p
  , Syntax.programFunctions = nativeFuns
  , Syntax.programForeignFunctions = foreignFuns
  }
  where
    (nativeFuns, foreignFuns) = runResolver (resolveFunctionDef mainFunDecl $ PreSyntax.programStatements p)
    mainFunDecl = PreSyntax.FunctionDecl Nothing (PreSyntax.FunctionName "main") []

data Layer = Layer
  { varEnv :: Map PreSyntax.VarName Syntax.VarID
  , funEnv :: Map PreSyntax.FunctionName Syntax.FunID
  }

emptyLayer :: Layer
emptyLayer =
  Layer
  { varEnv = Map.empty
  , funEnv = Map.empty
  }

newVariableInLayer :: Layer -> PreSyntax.VarName -> Syntax.VarID -> Maybe Layer
newVariableInLayer l@Layer {varEnv = lvars} vname vid =
  case Map.insertLookupWithKey (\_ val _ -> val) vname vid lvars of
    (Nothing, lvars') ->
      Just $
      l
      { varEnv = lvars'
      }
    (Just _, _) -> Nothing

findFunctionInLayer :: Layer -> PreSyntax.FunctionName -> Maybe Syntax.FunID
findFunctionInLayer Layer {funEnv = lfuns} fname = Map.lookup fname lfuns

newFunctionInLayer :: Layer -> PreSyntax.FunctionName -> Syntax.FunID -> Maybe Layer
newFunctionInLayer l@Layer {funEnv = lfuns} fname fid =
  case Map.insertLookupWithKey (\_ val _ -> val) fname fid lfuns of
    (Nothing, lfuns') ->
      Just $
      l
      { funEnv = lfuns'
      }
    (Just _, _) -> Nothing

data Function
  = NativeFunction Syntax.FunctionDef
  | ForeignFunction Syntax.ForeignFunctionDecl

data Env = Env
  { vars :: IntMap Syntax.VarType
  , funs :: IntMap (PreSyntax.FunctionDecl, Maybe Function)
  , lastVar :: Syntax.VarID
  , lastFun :: Syntax.FunID
  , layers :: [Layer]
  }

emptyEnv :: Env
emptyEnv = Env
  { vars = IntMap.empty
  , funs = IntMap.empty
  , lastVar = Syntax.VarID (-1)
  , lastFun = Syntax.FunID (-1)
  , layers = [emptyLayer]
  }

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

checkExistingFunctionInEnv :: Env -> Syntax.FunID -> PreSyntax.FunctionDecl -> (Syntax.FunID, Env)
checkExistingFunctionInEnv env (Syntax.FunID fid) fdecl =
  let Just (existingDecl, _) = IntMap.lookup fid $ funs env
  in if declMatch existingDecl fdecl
       then (Syntax.FunID fid, env)
       else error "Type mismatch"
  where
    declMatch (PreSyntax.FunctionDecl lrettype _ lparams) (PreSyntax.FunctionDecl rrettype _ rparams) =
      (lrettype == rrettype) && (getTypes lparams == getTypes rparams)
    getTypes = map (\(PreSyntax.VarDecl t _) -> t)

newFunctionInEnv :: Env -> PreSyntax.FunctionDecl -> (Syntax.FunID, Env)
newFunctionInEnv env fdecl =
  let (Syntax.FunID f) = inc $ lastFun env
      (Nothing, funs') =
        IntMap.insertLookupWithKey (\_ val _ -> val) f (fdecl, Nothing) (funs env)
  in ( Syntax.FunID f
     , env
       { lastFun = Syntax.FunID f
       , funs = funs'
       })
  where
    inc (Syntax.FunID lastFunID) = Syntax.FunID (lastFunID + 1)

introduceFunctionInEnv :: Env -> PreSyntax.FunctionDecl -> (Syntax.FunID, Env)
introduceFunctionInEnv env fdecl@(PreSyntax.FunctionDecl _ fname _) =
  case findFunctionInLayer (head $ layers env) fname of
    Just fid -> checkExistingFunctionInEnv env fid fdecl
    Nothing ->
      let (f, env'@Env {layers = l:ls}) = newFunctionInEnv env fdecl
          Just l' = newFunctionInLayer l fname f
      in ( f
         , env'
           { layers = l' : ls
           })

markFunctionAsDefinedInEnv :: Env -> Syntax.FunID -> Function -> Env
markFunctionAsDefinedInEnv env (Syntax.FunID fid) f =
  env
  { funs = go $ funs env
  }
  where
    go = IntMap.alter (\(Just (fdecl, Nothing)) -> Just (fdecl, Just f)) fid

findFunctionInEnv :: Env -> PreSyntax.FunctionName -> (Syntax.FunID, Maybe Function)
findFunctionInEnv env fname =
  let (Just f@(Syntax.FunID fid)) = asum (map (`findFunctionInLayer` fname) (layers env))
      (Just (_, mf)) = IntMap.lookup fid (funs env)
  in (f, mf)

type Resolver a = WriterT Syntax.AccessRecorder (State Env) a

runResolver :: Resolver () -> (IntMap Syntax.FunctionDef, IntMap Syntax.ForeignFunctionDecl)
runResolver m = (nativeFuns, foreignFuns)
  where
    finalEnv = execState (runWriterT m) emptyEnv
    foreignFuns = IntMap.mapMaybe asForeign $ funs finalEnv
    asForeign (_, Just (ForeignFunction f)) = Just f
    asForeign _ = Nothing
    nativeFuns = IntMap.mapMaybe asNative $ funs finalEnv
    asNative (_, Just (NativeFunction f)) = Just f
    asNative _ = Nothing

withRemoteVarAccess :: Resolver a -> Resolver (a, Syntax.AccessRecorder)
withRemoteVarAccess m = do
  (res, accesses) <- Writer.listen m
  let accessedVariables = Syntax.varAccess accesses
  accessibleVariables <- (toSet . concatMap (Map.elems . varEnv) . layers) <$> State.get
  let localAccessedVariables = accessedVariables `IntSet.difference` accessibleVariables
  let remoteAccessedVariables = accessedVariables `IntSet.difference` localAccessedVariables
  pure (res, accesses { Syntax.varAccess = remoteAccessedVariables })
  where
    toSet = IntSet.fromList . map (\(Syntax.VarID v) -> v)

recordVarAccess :: Syntax.VarID -> Resolver ()
recordVarAccess (Syntax.VarID v) = do
  Writer.tell $ Syntax.AccessRecorder
    { Syntax.varAccess = IntSet.singleton v
    , Syntax.funAccess = IntSet.empty
    , Syntax.foreignFunAccess = IntSet.empty
    }

recordForeignFunAccess :: Syntax.FunID -> Resolver ()
recordForeignFunAccess (Syntax.FunID v) = do
  Writer.tell $ Syntax.AccessRecorder
    { Syntax.foreignFunAccess = IntSet.singleton v
    , Syntax.funAccess = IntSet.empty
    , Syntax.varAccess = IntSet.empty
    }

recordFunAccess :: Syntax.FunID -> Resolver ()
recordFunAccess (Syntax.FunID v) = do
  Writer.tell $ Syntax.AccessRecorder
    { Syntax.funAccess = IntSet.singleton v
    , Syntax.foreignFunAccess = IntSet.empty
    , Syntax.varAccess = IntSet.empty
    }

usingVariable :: PreSyntax.VarName -> Resolver Syntax.VarID
usingVariable vname = do
  Just v <- (asum . map (Map.lookup vname . varEnv) . layers) <$> State.get
  recordVarAccess v
  pure v

newVariable :: Syntax.VarType -> Resolver Syntax.VarID
newVariable vtype = do
  envBefore <- State.get
  let (v, envAfter) = newVariableInEnv envBefore vtype
  State.put envAfter
  pure v

introduceVariable :: PreSyntax.VarDecl -> Resolver Syntax.VarID
introduceVariable vdecl = do
  envBefore <- State.get
  let (v, envAfter) = introduceVariableInEnv envBefore vdecl
  State.put envAfter
  pure v

introduceFunction :: PreSyntax.FunctionDecl -> Resolver Syntax.FunID
introduceFunction fdecl = do
  env <- State.get
  let (f, env') = introduceFunctionInEnv env fdecl
  State.put env'
  pure f

markFunctionAsDefined :: Syntax.FunID -> Function -> Resolver ()
markFunctionAsDefined fid f = State.modify (\env -> markFunctionAsDefinedInEnv env fid f)

findFunction :: PreSyntax.FunctionName -> Resolver (Syntax.FunID, Maybe Function)
findFunction fname = do
  env <- State.get
  pure $ findFunctionInEnv env fname

withLayer :: Resolver a -> Resolver a
withLayer m = do
  State.modify $ \env -> env { layers = emptyLayer : layers env }
  res <- m
  State.modify $ \env -> env { layers = tail $ layers env }
  pure res

resolveBlock :: [PreSyntax.Statement] -> Resolver Syntax.Block
resolveBlock stmts = withLayer $ do
  forM_ stmts scanStatement
  stmts' <- concat <$> mapM resolveStatement stmts
  l <- (head . layers) <$> State.get
  lvars <- variablesInLayer l
  pure $ Syntax.Block
    { Syntax.blockVariables = lvars
    , Syntax.blockStatements = stmts'
    }

scanStatement :: PreSyntax.Statement -> Resolver ()
scanStatement (PreSyntax.StatementVarDecl vdecl) = introduceVariable vdecl >> pure ()
scanStatement (PreSyntax.StatementVarDef vdecl _) = introduceVariable vdecl >> pure ()
scanStatement (PreSyntax.StatementFunctionDecl fdecl) = introduceFunction fdecl >> pure ()
scanStatement (PreSyntax.StatementFunctionDef fdecl _) = introduceFunction fdecl >> pure ()
scanStatement (PreSyntax.StatementForeignFunctionDecl fdecl) = resolveForeignFunctionDecl fdecl
scanStatement _ = pure ()

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
resolveStatement (PreSyntax.StatementVarDecl _) = pure []
resolveStatement (PreSyntax.StatementVarDef (PreSyntax.VarDecl _ vname) e) = do
  sequence
    [ Syntax.StatementAssign <$> usingVariable vname <*> resolveExpr e
    ]
resolveStatement (PreSyntax.StatementFunctionDecl _) = pure []
resolveStatement (PreSyntax.StatementAssign vname e) = do
  sequence [Syntax.StatementAssign <$> usingVariable vname <*> resolveExpr e]
resolveStatement (PreSyntax.StatementAssignPlus vname e) =
  sequence [Syntax.StatementAssign <$> usingVariable vname <*> resolveExpr e']
  where
    e' = PreSyntax.ExprPlus (PreSyntax.ExprVar vname) e
resolveStatement (PreSyntax.StatementAssignMinus vname e) =
  sequence [Syntax.StatementAssign <$> usingVariable vname <*> resolveExpr e']
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
resolveStatement (PreSyntax.StatementFor vname e1 e2 s) = resolveFor vname e1 e2 s
resolveStatement (PreSyntax.StatementFunctionDef fdecl stmts) = do
  resolveFunctionDef fdecl stmts
  pure []
resolveStatement (PreSyntax.StatementReturn Nothing) =
  sequence [pure $ Syntax.StatementReturn Nothing]
resolveStatement (PreSyntax.StatementReturn (Just e)) =
  sequence [(Syntax.StatementReturn . Just) <$> resolveExpr e]
resolveStatement (PreSyntax.StatementForeignFunctionDecl _) = pure []

resolveFunctionDef :: PreSyntax.FunctionDecl -> [PreSyntax.Statement] -> Resolver ()
resolveFunctionDef fdecl@(PreSyntax.FunctionDecl rettype _ params) stmts = do
  f <- introduceFunction fdecl
  ((params', body), accesses) <- withRemoteVarAccess $ withLayer $
    (,) <$> mapM introduceParam params <*> resolveBlock stmts
  markFunctionAsDefined f $ NativeFunction $ Syntax.FunctionDef
    { Syntax.funDefRetType = rettype
    , Syntax.funDefName = f
    , Syntax.funDefParams = params'
    , Syntax.funDefAccesses = accesses
    , Syntax.funDefBody = body
    }
  where
    introduceParam p@(PreSyntax.VarDecl t _) =
      (Syntax.VarDecl t) <$> introduceVariable p

resolveForeignFunctionDecl :: PreSyntax.FunctionDecl -> Resolver ()
resolveForeignFunctionDecl fdecl@(PreSyntax.FunctionDecl rettype (PreSyntax.FunctionName name) params) = do
  f <- introduceFunction fdecl
  markFunctionAsDefined f $ ForeignFunction $ Syntax.ForeignFunctionDecl
    { Syntax.foreignFunDeclRetType = rettype
    , Syntax.foreignFunDeclName = f
    , Syntax.foreignFunDeclRealName = name
    , Syntax.foreignFunDeclParams = map (\(PreSyntax.VarDecl vtype _) -> vtype) params
    }

resolveFor :: PreSyntax.VarName -> PreSyntax.Expr -> PreSyntax.Expr -> PreSyntax.Statement -> Resolver [Syntax.Statement]
resolveFor vname eFrom eTo s = do
  v <- usingVariable vname
  eFrom' <- resolveExpr eFrom
  eTo' <- resolveExpr eTo
  block <- resolveBlock [s]
  withLayer $ do
    vCur <- newVariable Syntax.VarTypeInt
    vTo <- newVariable Syntax.VarTypeInt
    let expr = Syntax.ExprNot (Syntax.ExprLt (Syntax.ExprVar vTo) (Syntax.ExprVar vCur))
        stmts =
          [ Syntax.StatementAssign v (Syntax.ExprVar vCur)
          , Syntax.StatementBlock block
          , Syntax.StatementAssign vCur (Syntax.ExprPlus (Syntax.ExprVar vCur) (Syntax.ExprInt 1))
          ]
        block' = Syntax.Block { Syntax.blockVariables = [], Syntax.blockStatements = stmts }
    pure [Syntax.StatementBlock $ Syntax.Block
      { Syntax.blockVariables =
        [ Syntax.VarDecl Syntax.VarTypeInt vCur
        , Syntax.VarDecl Syntax.VarTypeInt vTo
        ]
      , Syntax.blockStatements =
        [ Syntax.StatementAssign vCur eFrom'
        , Syntax.StatementAssign vTo eTo'
        , Syntax.StatementWhile expr block'
        ]
      }]

resolveFunctionCall :: PreSyntax.FunctionCall -> Resolver Syntax.FunctionCall
resolveFunctionCall (PreSyntax.FunctionCall (PreSyntax.FunctionName "print") args) =
  Syntax.PrintCall <$> mapM resolveExpr args
resolveFunctionCall (PreSyntax.FunctionCall fname args) = do
  (fid, mf) <- findFunction fname
  case mf of
    Just (ForeignFunction f) -> do
      recordForeignFunAccess fid
      Syntax.ForeignFunctionCall (Syntax.foreignFunDeclName f) <$> mapM resolveExpr args
    Just (NativeFunction _) -> do
      recordFunAccess fid
      Syntax.NativeFunctionCall fid <$> mapM resolveExpr args
    Nothing -> do -- Because of prescanning in the block, this is definetely not a foreign call.
      recordFunAccess fid
      Syntax.NativeFunctionCall fid <$> mapM resolveExpr args

resolveExpr :: PreSyntax.Expr -> Resolver Syntax.Expr
resolveExpr (PreSyntax.ExprFunctionCall fcall) =
  Syntax.ExprFunctionCall <$> resolveFunctionCall fcall
resolveExpr (PreSyntax.ExprVar vname) = Syntax.ExprVar <$> usingVariable vname
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
