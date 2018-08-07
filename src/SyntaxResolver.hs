module SyntaxResolver
  ( resolve
  ) where

import Control.Monad
import Control.Monad.State (State, execState)
import qualified Control.Monad.State as State
import qualified Control.Monad.Trans as Trans
import Control.Monad.Writer (WriterT, execWriterT)
import qualified Control.Monad.Writer as Writer
import Data.Foldable (asum)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map

import qualified PreSyntax
import qualified ResolvedSyntax
import qualified VarUsageResolver

resolve :: PreSyntax.Program -> ResolvedSyntax.Program
resolve p =
  ResolvedSyntax.Program
    { ResolvedSyntax.programLibraries = PreSyntax.programLibraries p
    , ResolvedSyntax.programFunctions = nativeFuns
    , ResolvedSyntax.programForeignFunctions = foreignFuns
    , ResolvedSyntax.programStrings = strings finalEnv
    , ResolvedSyntax.programVariables = vars finalEnv
    , ResolvedSyntax.programLastFunID = lastFun finalEnv
    , ResolvedSyntax.programLastVarID = lastVar finalEnv
    , ResolvedSyntax.programLastStringID = lastString finalEnv
    }
  where
    mainFunDecl =
      PreSyntax.FunctionDecl Nothing (PreSyntax.FunctionName "main") []
    finalEnv =
      runResolver $ do
        resolveFunctionDef mainFunDecl $ PreSyntax.programStatements p
        resolveCaptures
    foreignFuns = IntMap.mapMaybe asForeign $ funs finalEnv
    asForeign (_, Just (ForeignFunction f)) = Just f
    asForeign _ = Nothing
    nativeFuns = IntMap.mapMaybe asNative $ funs finalEnv
    asNative (_, Just (NativeFunction (NativeFunctionImpl f _))) = Just f
    asNative _ = Nothing

data Layer = Layer
  { varEnv :: Map PreSyntax.VarName ResolvedSyntax.VarID
  , funEnv :: Map PreSyntax.FunctionName ResolvedSyntax.FunID
  }

emptyLayer :: Layer
emptyLayer = Layer {varEnv = Map.empty, funEnv = Map.empty}

newVariableInLayer ::
     Layer -> PreSyntax.VarName -> ResolvedSyntax.VarID -> Maybe Layer
newVariableInLayer l@Layer {varEnv = lvars} vname vid =
  case Map.insertLookupWithKey (\_ val _ -> val) vname vid lvars of
    (Nothing, lvars') -> Just $ l {varEnv = lvars'}
    (Just _, _) -> Nothing

findFunctionInLayer ::
     Layer -> PreSyntax.FunctionName -> Maybe ResolvedSyntax.FunID
findFunctionInLayer Layer {funEnv = lfuns} fname = Map.lookup fname lfuns

newFunctionInLayer ::
     Layer -> PreSyntax.FunctionName -> ResolvedSyntax.FunID -> Maybe Layer
newFunctionInLayer l@Layer {funEnv = lfuns} fname fid =
  case Map.insertLookupWithKey (\_ val _ -> val) fname fid lfuns of
    (Nothing, lfuns') -> Just $ l {funEnv = lfuns'}
    (Just _, _) -> Nothing

data NativeFunctionImpl =
  NativeFunctionImpl ResolvedSyntax.FunctionDef
                     ResolverLog

data Function
  = NativeFunction NativeFunctionImpl
  | ForeignFunction ResolvedSyntax.ForeignFunctionDecl

data Env = Env
  { vars :: IntMap ResolvedSyntax.VarType
  , funs :: IntMap (PreSyntax.FunctionDecl, Maybe Function)
  , strings :: IntMap String
  , lastVar :: ResolvedSyntax.VarID
  , lastFun :: ResolvedSyntax.FunID
  , lastString :: ResolvedSyntax.StringID
  , layers :: [Layer]
  }

emptyEnv :: Env
emptyEnv =
  Env
    { vars = IntMap.empty
    , funs = IntMap.empty
    , strings = IntMap.empty
    , lastVar = ResolvedSyntax.VarID (-1)
    , lastFun = ResolvedSyntax.FunID (-1)
    , lastString = ResolvedSyntax.StringID (-1)
    , layers = [emptyLayer]
    }

newVariableInEnv :: Env -> ResolvedSyntax.VarType -> (ResolvedSyntax.VarID, Env)
newVariableInEnv env vtype =
  let (ResolvedSyntax.VarID v) = inc $ lastVar env
      (Nothing, vars') =
        IntMap.insertLookupWithKey (\_ val _ -> val) v vtype (vars env)
   in ( ResolvedSyntax.VarID v
      , env {lastVar = ResolvedSyntax.VarID v, vars = vars'})
  where
    inc (ResolvedSyntax.VarID lastVarID) = ResolvedSyntax.VarID (lastVarID + 1)

introduceVariableInEnv ::
     Env -> PreSyntax.VarDecl -> (ResolvedSyntax.VarID, Env)
introduceVariableInEnv env (PreSyntax.VarDecl vtype vname) =
  let (v, env'@Env {layers = (l:ls)}) = newVariableInEnv env vtype
      Just l' = newVariableInLayer l vname v
   in (v, env' {layers = l' : ls})

checkExistingNativeFunctionInEnv ::
     Env
  -> ResolvedSyntax.FunID
  -> PreSyntax.FunctionDecl
  -> (ResolvedSyntax.FunID, Env)
checkExistingNativeFunctionInEnv env (ResolvedSyntax.FunID fid) fdecl =
  let Just (existingDecl, f) = IntMap.lookup fid $ funs env
   in case f of
        Just (ForeignFunction _) ->
          error "Foreign functions cannot be forward declared"
        _ ->
          if declMatch existingDecl fdecl
            then (ResolvedSyntax.FunID fid, env)
            else error "Type mismatch"
  where
    declMatch (PreSyntax.FunctionDecl lrettype _ lparams) (PreSyntax.FunctionDecl rrettype _ rparams) =
      (lrettype == rrettype) && (getTypes lparams == getTypes rparams)
    getTypes = map (\(PreSyntax.VarDecl t _) -> t)

newFunctionInEnv ::
     Env
  -> PreSyntax.FunctionDecl
  -> (ResolvedSyntax.FunID -> Maybe Function)
  -> (ResolvedSyntax.FunID, Env)
newFunctionInEnv env fdecl mf =
  let (ResolvedSyntax.FunID f) = inc $ lastFun env
      (Nothing, funs') =
        IntMap.insertLookupWithKey
          (\_ val _ -> val)
          f
          (fdecl, mf (ResolvedSyntax.FunID f))
          (funs env)
   in ( ResolvedSyntax.FunID f
      , env {lastFun = ResolvedSyntax.FunID f, funs = funs'})
  where
    inc (ResolvedSyntax.FunID lastFunID) = ResolvedSyntax.FunID (lastFunID + 1)

introduceNativeFunctionInEnv ::
     Env -> PreSyntax.FunctionDecl -> (ResolvedSyntax.FunID, Env)
introduceNativeFunctionInEnv env fdecl@(PreSyntax.FunctionDecl _ fname _) =
  case findFunctionInLayer (head $ layers env) fname of
    Just fid -> checkExistingNativeFunctionInEnv env fid fdecl
    Nothing ->
      let (f, env'@Env {layers = l:ls}) =
            newFunctionInEnv env fdecl (const Nothing)
          Just l' = newFunctionInLayer l fname f
       in (f, env' {layers = l' : ls})

markNativeFunctionAsDefinedInEnv ::
     Env -> ResolvedSyntax.FunID -> NativeFunctionImpl -> Env
markNativeFunctionAsDefinedInEnv env (ResolvedSyntax.FunID fid) f =
  env {funs = go $ funs env}
  where
    go =
      IntMap.alter
        (\(Just (fdecl, Nothing)) -> Just (fdecl, Just $ NativeFunction f))
        fid

introduceForeignFunctionInEnv ::
     Env
  -> PreSyntax.FunctionDecl
  -> (ResolvedSyntax.FunID -> ResolvedSyntax.ForeignFunctionDecl)
  -> Env
introduceForeignFunctionInEnv env fdecl@(PreSyntax.FunctionDecl _ fname _) ffdecl =
  let (f, env'@Env {layers = l:ls}) =
        newFunctionInEnv env fdecl (Just . ForeignFunction . ffdecl)
      Just l' = newFunctionInLayer l fname f
   in env' {layers = l' : ls}

findFunctionInEnv ::
     Env -> PreSyntax.FunctionName -> (ResolvedSyntax.FunID, Maybe Function)
findFunctionInEnv env fname =
  let (Just f@(ResolvedSyntax.FunID fid)) =
        asum (map (`findFunctionInLayer` fname) (layers env))
      (Just (_, mf)) = IntMap.lookup fid (funs env)
   in (f, mf)

introduceStringInEnv :: Env -> String -> (ResolvedSyntax.StringID, Env)
introduceStringInEnv env val =
  let (ResolvedSyntax.StringID sid) = inc $ lastString env
      (Nothing, strings') =
        IntMap.insertLookupWithKey (\_ v _ -> v) sid val (strings env)
   in ( ResolvedSyntax.StringID sid
      , env {lastString = ResolvedSyntax.StringID sid, strings = strings'})
  where
    inc (ResolvedSyntax.StringID lastStringID) =
      ResolvedSyntax.StringID (lastStringID + 1)

data ResolverLog = ResolverLog
  { locals :: [ResolvedSyntax.VarDecl]
  , varAccess :: [ResolvedSyntax.VarID]
  , funAccess :: [ResolvedSyntax.FunID]
  }

instance Monoid ResolverLog where
  mempty = ResolverLog {locals = mempty, varAccess = mempty, funAccess = mempty}
  lhs `mappend` rhs =
    ResolverLog
      { locals = locals lhs `mappend` locals rhs
      , varAccess = varAccess lhs `mappend` varAccess rhs
      , funAccess = funAccess lhs `mappend` funAccess rhs
      }

type Resolver = WriterT ResolverLog (State Env)

runResolver :: Resolver () -> Env
runResolver m = execState (execWriterT m) emptyEnv

resolveVariable :: PreSyntax.VarName -> Resolver ResolvedSyntax.VarID
resolveVariable vname = do
  Just v <- State.gets (asum . map (Map.lookup vname . varEnv) . layers)
  Writer.tell $ mempty {varAccess = [v]}
  pure v

updateVariable ::
     (Env -> (ResolvedSyntax.VarID, ResolvedSyntax.VarType, Env))
  -> Resolver ResolvedSyntax.VarID
updateVariable action = do
  envBefore <- State.get
  let (v, vtype, envAfter) = action envBefore
  State.put envAfter
  Writer.tell $ mempty {locals = [ResolvedSyntax.VarDecl vtype v]}
  pure v

newVariable :: ResolvedSyntax.VarType -> Resolver ResolvedSyntax.VarID
newVariable vtype =
  updateVariable
    (\envBefore ->
       let (v, envAfter) = newVariableInEnv envBefore vtype
        in (v, vtype, envAfter))

introduceVariable :: PreSyntax.VarDecl -> Resolver ResolvedSyntax.VarID
introduceVariable vdecl@(PreSyntax.VarDecl vtype _) =
  updateVariable
    (\envBefore ->
       let (v, envAfter) = introduceVariableInEnv envBefore vdecl
        in (v, vtype, envAfter))

introduceNativeFunction ::
     PreSyntax.FunctionDecl -> Resolver ResolvedSyntax.FunID
introduceNativeFunction fdecl = do
  env <- State.get
  let (f, env') = introduceNativeFunctionInEnv env fdecl
  State.put env'
  pure f

introduceForeignFunction ::
     PreSyntax.FunctionDecl
  -> (ResolvedSyntax.FunID -> ResolvedSyntax.ForeignFunctionDecl)
  -> Resolver ()
introduceForeignFunction fdecl ffdecl = do
  env <- State.get
  let env' = introduceForeignFunctionInEnv env fdecl ffdecl
  State.put env'

markNativeFunctionAsDefined ::
     ResolvedSyntax.FunID -> NativeFunctionImpl -> Resolver ()
markNativeFunctionAsDefined fid f =
  State.modify (\env -> markNativeFunctionAsDefinedInEnv env fid f)

findFunction ::
     PreSyntax.FunctionName -> Resolver (ResolvedSyntax.FunID, Maybe Function)
findFunction fname = do
  env <- State.get
  pure $ findFunctionInEnv env fname

introduceString :: String -> Resolver ResolvedSyntax.StringID
introduceString val = do
  env <- State.get
  let (cid, env') = introduceStringInEnv env val
  State.put env'
  pure cid

withLayer :: Resolver a -> Resolver a
withLayer m = do
  State.modify $ \env -> env {layers = emptyLayer : layers env}
  res <- m
  State.modify $ \env -> env {layers = tail $ layers env}
  pure res

resolveBlock :: [PreSyntax.Statement] -> Resolver ResolvedSyntax.Block
resolveBlock stmts =
  withLayer $ do
    stmts' <- resolveStatements stmts
    pure ResolvedSyntax.Block {ResolvedSyntax.blockStatements = stmts'}

type StatementResolver = WriterT [ResolvedSyntax.Statement] Resolver

resolveStatements ::
     [PreSyntax.Statement] -> Resolver [ResolvedSyntax.Statement]
resolveStatements stmts = execWriterT $ forM_ stmts resolveStatement

addStmt :: ResolvedSyntax.Statement -> StatementResolver ()
addStmt stmt = Writer.tell [stmt]

resolveStatement :: PreSyntax.Statement -> StatementResolver ()
resolveStatement (PreSyntax.StatementBlock b) = do
  b' <- Trans.lift $ resolveBlock b
  addStmt $ ResolvedSyntax.StatementBlock b'
resolveStatement (PreSyntax.StatementFunctionCall fcall) = do
  fcall' <- Trans.lift $ resolveFunctionCall fcall
  addStmt $ ResolvedSyntax.StatementFunctionCall fcall'
resolveStatement (PreSyntax.StatementWhile e stmt) = do
  e' <- Trans.lift $ resolveExpr e
  b <- Trans.lift $ resolveBlock [stmt]
  addStmt $ ResolvedSyntax.StatementWhile e' b
resolveStatement (PreSyntax.StatementVarDecl vdecl) = do
  v <- Trans.lift $ introduceVariable vdecl
  addStmt $ ResolvedSyntax.StatementVarAlloc v
resolveStatement (PreSyntax.StatementVarDef vdecl e) = do
  v <- Trans.lift $ introduceVariable vdecl
  addStmt $ ResolvedSyntax.StatementVarAlloc v
  e' <- Trans.lift $ resolveExpr e
  addStmt $ ResolvedSyntax.StatementAssign v e'
resolveStatement (PreSyntax.StatementFunctionDecl fdecl) = do
  _ <- Trans.lift $ introduceNativeFunction fdecl
  pure ()
resolveStatement (PreSyntax.StatementAssign vname e) = do
  v <- Trans.lift $ resolveVariable vname
  e' <- Trans.lift $ resolveExpr e
  addStmt $ ResolvedSyntax.StatementAssign v e'
resolveStatement (PreSyntax.StatementAssignPlus vname e) = do
  v <- Trans.lift $ resolveVariable vname
  e' <- Trans.lift $ resolveExpr e
  addStmt $ ResolvedSyntax.StatementAssignPlus v e'
resolveStatement (PreSyntax.StatementAssignMinus vname e) = do
  v <- Trans.lift $ resolveVariable vname
  e' <- Trans.lift $ resolveExpr e
  addStmt $ ResolvedSyntax.StatementAssignMinus v e'
resolveStatement (PreSyntax.StatementIfElse e s1 s2) = do
  e' <- Trans.lift $ resolveExpr e
  bt <- Trans.lift $ resolveBlock [s1]
  bf <- Trans.lift $ resolveBlock [s2]
  addStmt $ ResolvedSyntax.StatementIfElse e' bt bf
resolveStatement (PreSyntax.StatementIf e s) = do
  e' <- Trans.lift $ resolveExpr e
  b <- Trans.lift $ resolveBlock [s]
  addStmt $ ResolvedSyntax.StatementIf e' b
resolveStatement (PreSyntax.StatementFor vname e1 e2 s) = do
  stmt <- Trans.lift $ resolveFor vname e1 e2 s
  addStmt stmt
resolveStatement (PreSyntax.StatementFunctionDef fdecl stmts) = do
  _ <- Trans.lift $ introduceNativeFunction fdecl
  Trans.lift $ resolveFunctionDef fdecl stmts
  pure ()
resolveStatement (PreSyntax.StatementReturn Nothing) =
  addStmt $ ResolvedSyntax.StatementReturn Nothing
resolveStatement (PreSyntax.StatementReturn (Just e)) = do
  e' <- Trans.lift $ resolveExpr e
  addStmt $ ResolvedSyntax.StatementReturn $ Just e'
resolveStatement (PreSyntax.StatementForeignFunctionDecl fdecl) =
  Trans.lift $ resolveForeignFunctionDecl fdecl

resolveFunctionDef ::
     PreSyntax.FunctionDecl -> [PreSyntax.Statement] -> Resolver ()
resolveFunctionDef fdecl@(PreSyntax.FunctionDecl rettype _ params) stmts = do
  f <- introduceNativeFunction fdecl
  ((params', body), usages) <-
    withLayer $
    Writer.listen $ (,) <$> mapM introduceParam params <*> resolveBlock stmts
  markNativeFunctionAsDefined f $
    NativeFunctionImpl
      ResolvedSyntax.FunctionDef
        { ResolvedSyntax.funDefRetType = rettype
        , ResolvedSyntax.funDefName = f
        , ResolvedSyntax.funDefParams = params'
        , ResolvedSyntax.funDefCaptures = [] -- Will be filled out later.
        , ResolvedSyntax.funDefBody = body
        }
      usages
  where
    introduceParam p@(PreSyntax.VarDecl t _) =
      ResolvedSyntax.VarDecl t <$> introduceVariable p

resolveForeignFunctionDecl :: PreSyntax.FunctionDecl -> Resolver ()
resolveForeignFunctionDecl fdecl@(PreSyntax.FunctionDecl rettype (PreSyntax.FunctionName name) params) =
  introduceForeignFunction fdecl $ \f ->
    ResolvedSyntax.ForeignFunctionDecl
      { ResolvedSyntax.foreignFunDeclRetType = rettype
      , ResolvedSyntax.foreignFunDeclName = f
      , ResolvedSyntax.foreignFunDeclRealName = name
      , ResolvedSyntax.foreignFunDeclParams =
          map (\(PreSyntax.VarDecl vtype _) -> vtype) params
      }

resolveFor ::
     PreSyntax.VarName
  -> PreSyntax.Expr
  -> PreSyntax.Expr
  -> PreSyntax.Statement
  -> Resolver ResolvedSyntax.Statement
resolveFor vname eFrom eTo s = do
  v <- resolveVariable vname
  eFrom' <- resolveExpr eFrom
  eTo' <- resolveExpr eTo
  block <- resolveBlock [s]
  withLayer $ do
    vCur <- newVariable ResolvedSyntax.VarTypeInt
    vTo <- newVariable ResolvedSyntax.VarTypeInt
    let expr =
          ResolvedSyntax.ExprNot
            (ResolvedSyntax.ExprLt
               (ResolvedSyntax.ExprVar vTo)
               (ResolvedSyntax.ExprVar vCur))
        stmts =
          [ ResolvedSyntax.StatementAssign v (ResolvedSyntax.ExprVar vCur)
          , ResolvedSyntax.StatementBlock block
          , ResolvedSyntax.StatementAssign
              vCur
              (ResolvedSyntax.ExprPlus
                 (ResolvedSyntax.ExprVar vCur)
                 (ResolvedSyntax.ExprConst (ResolvedSyntax.ImmediateInt 1)))
          ]
        block' = ResolvedSyntax.Block {ResolvedSyntax.blockStatements = stmts}
    pure $
      ResolvedSyntax.StatementBlock
        ResolvedSyntax.Block
          { ResolvedSyntax.blockStatements =
              [ ResolvedSyntax.StatementVarAlloc vCur
              , ResolvedSyntax.StatementVarAlloc vTo
              , ResolvedSyntax.StatementAssign vCur eFrom'
              , ResolvedSyntax.StatementAssign vTo eTo'
              , ResolvedSyntax.StatementWhile expr block'
              ]
          }

resolveFunctionCall ::
     PreSyntax.FunctionCall -> Resolver ResolvedSyntax.FunctionCall
resolveFunctionCall (PreSyntax.FunctionCall (PreSyntax.FunctionName "print") args) =
  ResolvedSyntax.PrintCall <$> mapM resolveExpr args
resolveFunctionCall (PreSyntax.FunctionCall fname args) = do
  (fid, mf) <- findFunction fname
  case mf of
    Just (ForeignFunction f) ->
      ResolvedSyntax.ForeignFunctionCall (ResolvedSyntax.foreignFunDeclName f) <$>
      mapM resolveExpr args
    Just NativeFunction {} -> do
      Writer.tell $ mempty {funAccess = [fid]}
      ResolvedSyntax.NativeFunctionCall fid <$> mapM resolveExpr args
    Nothing -- We do not allow forward declaring foreign functions, so this is definetely a native call.
     -> do
      Writer.tell $ mempty {funAccess = [fid]}
      ResolvedSyntax.NativeFunctionCall fid <$> mapM resolveExpr args

resolveExpr :: PreSyntax.Expr -> Resolver ResolvedSyntax.Expr
resolveExpr (PreSyntax.ExprFunctionCall fcall) =
  ResolvedSyntax.ExprFunctionCall <$> resolveFunctionCall fcall
resolveExpr (PreSyntax.ExprVar vname) =
  ResolvedSyntax.ExprVar <$> resolveVariable vname
resolveExpr (PreSyntax.ExprInt i) =
  pure $ ResolvedSyntax.ExprConst $ ResolvedSyntax.ImmediateInt i
resolveExpr (PreSyntax.ExprFloat f) =
  pure $ ResolvedSyntax.ExprConst $ ResolvedSyntax.ImmediateFloat f
resolveExpr (PreSyntax.ExprString s) =
  (ResolvedSyntax.ExprConst . ResolvedSyntax.ImmediateString) <$>
  introduceString s
resolveExpr (PreSyntax.ExprNeg e) = ResolvedSyntax.ExprNeg <$> resolveExpr e
resolveExpr (PreSyntax.ExprPlus lhs rhs) =
  ResolvedSyntax.ExprPlus <$> resolveExpr lhs <*> resolveExpr rhs
resolveExpr (PreSyntax.ExprMinus lhs rhs) =
  ResolvedSyntax.ExprMinus <$> resolveExpr lhs <*> resolveExpr rhs
resolveExpr (PreSyntax.ExprTimes lhs rhs) =
  ResolvedSyntax.ExprTimes <$> resolveExpr lhs <*> resolveExpr rhs
resolveExpr (PreSyntax.ExprDiv lhs rhs) =
  ResolvedSyntax.ExprDiv <$> resolveExpr lhs <*> resolveExpr rhs
resolveExpr (PreSyntax.ExprMod lhs rhs) =
  ResolvedSyntax.ExprMod <$> resolveExpr lhs <*> resolveExpr rhs
resolveExpr (PreSyntax.ExprBitAnd lhs rhs) =
  ResolvedSyntax.ExprBitAnd <$> resolveExpr lhs <*> resolveExpr rhs
resolveExpr (PreSyntax.ExprBitOr lhs rhs) =
  ResolvedSyntax.ExprBitOr <$> resolveExpr lhs <*> resolveExpr rhs
resolveExpr (PreSyntax.ExprBitXor lhs rhs) =
  ResolvedSyntax.ExprBitXor <$> resolveExpr lhs <*> resolveExpr rhs
resolveExpr (PreSyntax.ExprNot e) = ResolvedSyntax.ExprNot <$> resolveExpr e
resolveExpr (PreSyntax.ExprAnd lhs rhs) =
  ResolvedSyntax.ExprAnd <$> resolveExpr lhs <*> resolveExpr rhs
resolveExpr (PreSyntax.ExprOr lhs rhs) =
  ResolvedSyntax.ExprOr <$> resolveExpr lhs <*> resolveExpr rhs
resolveExpr (PreSyntax.ExprEq lhs rhs) =
  ResolvedSyntax.ExprEq <$> resolveExpr lhs <*> resolveExpr rhs
resolveExpr (PreSyntax.ExprNeq lhs rhs) =
  ResolvedSyntax.ExprNeq <$> resolveExpr lhs <*> resolveExpr rhs
resolveExpr (PreSyntax.ExprLt lhs rhs) =
  ResolvedSyntax.ExprLt <$> resolveExpr lhs <*> resolveExpr rhs
resolveExpr (PreSyntax.ExprLeq lhs rhs) =
  ResolvedSyntax.ExprLeq <$> resolveExpr lhs <*> resolveExpr rhs
resolveExpr (PreSyntax.ExprGt lhs rhs) =
  ResolvedSyntax.ExprGt <$> resolveExpr lhs <*> resolveExpr rhs
resolveExpr (PreSyntax.ExprGeq lhs rhs) =
  ResolvedSyntax.ExprGeq <$> resolveExpr lhs <*> resolveExpr rhs

resolveCaptures :: Resolver ()
resolveCaptures = do
  entries <- State.gets (IntMap.mapMaybe asNative . funs)
  let varUsages =
        IntMap.map (map ResolvedSyntax.VarID) $
        VarUsageResolver.resolveVarUsage entries
  State.modify $ \env ->
    env {funs = IntMap.mapWithKey (modifyFun varUsages) $ funs env}
  where
    asNative (_, Just (NativeFunction (NativeFunctionImpl _ usages))) =
      Just $
      VarUsageResolver.UsageEntry
        { VarUsageResolver.vars =
            map (\(ResolvedSyntax.VarID i) -> i) $ varAccess usages
        , VarUsageResolver.locals =
            map (\(ResolvedSyntax.VarDecl _ (ResolvedSyntax.VarID i)) -> i) $
            locals usages
        , VarUsageResolver.funs =
            map (\(ResolvedSyntax.FunID i) -> i) $ funAccess usages
        }
    asNative _ =
      Just $
      VarUsageResolver.UsageEntry
        { VarUsageResolver.vars = []
        , VarUsageResolver.locals = []
        , VarUsageResolver.funs = []
        }
    modifyFun ::
         IntMap [ResolvedSyntax.VarID]
      -> Int
      -> (PreSyntax.FunctionDecl, Maybe Function)
      -> (PreSyntax.FunctionDecl, Maybe Function)
    modifyFun varUsages fid (fdecl, Just (NativeFunction (NativeFunctionImpl f usages))) =
      ( fdecl
      , Just
          (NativeFunction
             (NativeFunctionImpl
                (updateCaptures f (varUsages IntMap.! fid) (locals usages))
                usages)))
    modifyFun _ _ f = f
    updateCaptures ::
         ResolvedSyntax.FunctionDef
      -> [ResolvedSyntax.VarID]
      -> [ResolvedSyntax.VarDecl]
      -> ResolvedSyntax.FunctionDef
    updateCaptures f usages localVars =
      f
        { ResolvedSyntax.funDefCaptures =
            usages List.\\ map (\(ResolvedSyntax.VarDecl _ v) -> v) localVars
        }
