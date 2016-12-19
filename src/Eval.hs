{-# LANGUAGE LambdaCase #-}

module Eval
  ( eval
  ) where

import Control.Monad (forM_, when)
import Control.Monad.State (StateT, runStateT)
import qualified Control.Monad.State as State
import Control.Monad.Except (ExceptT, runExceptT)
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Trans as Trans
import Data.Foldable (asum)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map

import ForeignEval
import Syntax
import Value

eval :: Program -> IO ()
eval (Program stmts) = do
  (finalEnv, _) <- runExecute emptyEnv (execute (StatementBlock stmts))
  forM_ (envLibs finalEnv) dlclose
  pure ()

valueToExpr :: Value -> Expr
valueToExpr (ValueInt i) = ExprInt i
valueToExpr (ValueFloat f) = ExprFloat f
valueToExpr (ValueString s) = ExprString s

type LayerID = Int

data FunctionImpl
  = FunctionBody (LayerID, [Statement])
  | FunctionForeign ForeignFun

data Function =
  Function (Maybe VarType)
           [VarDecl]
           (Maybe FunctionImpl)

argsMatch :: [VarDecl] -> [VarDecl] -> Bool
argsMatch largs rargs =
  map (\(VarDecl t _) -> t) largs == map (\(VarDecl t _) -> t) rargs

functionTypesMatch :: Function -> Function -> Bool
functionTypesMatch (Function lrettype lparams _) (Function rrettype rparams _) =
  lrettype == rrettype && argsMatch lparams rparams

data Layer = Layer
  { varEnv :: Map VarName Value
  , funEnv :: Map FunctionName Function
  }

emptyLayer :: Layer
emptyLayer = Layer Map.empty Map.empty

readVariableFromLayer :: Layer -> VarName -> Maybe Value
readVariableFromLayer l name = Map.lookup name (varEnv l)

writeVariableToLayer :: Layer -> VarName -> Value -> Maybe Layer
writeVariableToLayer l name value =
  let (moldvalue, newvarenv) =
        Map.insertLookupWithKey
          (\_ newVal oldVal -> convert newVal (typeof oldVal))
          name
          value
          (varEnv l)
  in case moldvalue of
       Nothing -> Nothing
       Just _ ->
         Just $
         l
         { varEnv = newvarenv
         }

addVariableToLayer :: Layer -> VarName -> VarType -> Maybe Layer
addVariableToLayer l name vtype =
  let (oldvalue, newvarenv) =
        Map.insertLookupWithKey
          (\_ a _ -> a)
          name
          (defaultValueFromType vtype)
          (varEnv l)
  in maybe
       (Just $
        l
        { varEnv = newvarenv
        })
       (const Nothing)
       oldvalue

getFunctionFromLayer :: Layer -> FunctionName -> Maybe Function
getFunctionFromLayer l name = Map.lookup name (funEnv l)

addFunctionToLayer :: Layer -> FunctionName -> Function -> Maybe Layer
addFunctionToLayer l name f@(Function _ _ body) =
  let (oldvalue, newfunenv) =
        Map.insertLookupWithKey (\_ a _ -> a) name f (funEnv l)
  in case (oldvalue, body) of
       (Nothing, _) ->
         Just $
         l
         { funEnv = newfunenv
         }
       (Just (Function _ _ (Just _)), Just _) -> Nothing
       (Just oldf@(Function _ _ (Just _)), Nothing)
         | functionTypesMatch oldf f -> Just l
         | otherwise -> Nothing
       (Just oldf@(Function _ _ Nothing), _)
         | functionTypesMatch oldf f ->
           Just $
           l
           { funEnv = newfunenv
           }
         | otherwise -> Nothing

data Env = Env
  { envLibs :: [LibHandle]
  , envLayers :: [Layer]
  }

emptyEnv :: Env
emptyEnv = Env [] []

modifyingLayer :: (Layer -> Maybe Layer) -> [Layer] -> Maybe [Layer]
modifyingLayer _ [] = Nothing
modifyingLayer f (l:ls) =
  case f l of
    Nothing -> (l :) <$> modifyingLayer f ls
    Just l' -> Just (l' : ls)

readVariableFromEnv :: Env -> VarName -> Maybe Value
readVariableFromEnv Env {envLayers = curlayer:otherlayers} vname =
  asum (map (`readVariableFromLayer` vname) (curlayer : otherlayers))
readVariableFromEnv _ _ = error "Env broke"

writeVariableToEnv :: Env -> VarName -> Value -> Maybe Env
writeVariableToEnv env@Env {envLayers = curlayer:otherlayers} vname val =
  case modifyingLayer
         (\l -> writeVariableToLayer l vname val)
         (curlayer : otherlayers) of
    Just (curlayer':otherlayers') ->
      Just
        (env
         { envLayers = curlayer' : otherlayers'
         })
    Just _ -> error "Impossible"
    Nothing -> Nothing
writeVariableToEnv _ _ _ = error "Env broke"

addVariableToEnv :: Env -> VarName -> VarType -> Maybe Env
addVariableToEnv env@Env {envLayers = curlayer:otherlayers} vname vtype =
  case addVariableToLayer curlayer vname vtype of
    Nothing -> Nothing
    Just l' ->
      Just
        (env
         { envLayers = l' : otherlayers
         })
addVariableToEnv _ _ _ = error "Env broke"

getFunctionFromEnv :: Env -> FunctionName -> Maybe Function
getFunctionFromEnv Env {envLayers = curlayer:otherlayers} name =
  asum (map (`getFunctionFromLayer` name) (curlayer : otherlayers))
getFunctionFromEnv _ _ = error "Env broke"

addFunctionToEnv :: Env -> FunctionName -> Function -> Maybe Env
addFunctionToEnv env@Env {envLayers = curlayer:otherlayers} fname f =
  case addFunctionToLayer curlayer fname f of
    Nothing -> Nothing
    Just l' ->
      Just $
      env
      { envLayers = l' : otherlayers
      }
addFunctionToEnv _ _ _ = error "Env broke"

type Execute a = ExceptT (Maybe Value) (StateT Env IO) a

withNewLayer :: Execute a -> Execute a
withNewLayer m = do
  lid <- currentLayer
  newLayer
  res <- m
  dropLayer
  newLid <- currentLayer
  when (lid /= newLid) $ error "Scoping broke."
  pure res
  where
    newLayer =
      State.modify
        (\env@Env {envLayers = layers} ->
            env
            { envLayers = emptyLayer : layers
            })
    dropLayer =
      State.modify
        (\env@Env {envLayers = layers} ->
            env
            { envLayers = tail layers
            })

currentLayer :: Execute LayerID
currentLayer = (length . envLayers) <$> State.get

splitLayers :: LayerID -> Execute [Layer]
splitLayers lid = do
  env@Env {envLayers = ls} <- State.get
  let (newenv, preserve) = List.splitAt lid $ List.reverse ls
  State.put $
    env
    { envLayers = List.reverse newenv
    }
  pure $ List.reverse preserve

combineLayers :: [Layer] -> Execute ()
combineLayers ls =
  State.modify
    (\env@Env {envLayers = layers} ->
        env
        { envLayers = ls ++ layers
        })

withLayer :: LayerID -> Execute a -> Execute a
withLayer lid m = do
  oldLid <- currentLayer
  preserve <- splitLayers lid
  res <- m
  combineLayers preserve
  newLid <- currentLayer
  when (oldLid /= newLid) $ error "Scoping broke."
  pure res

openLibrary :: String -> Execute ()
openLibrary lib = do
  handle <- either error id <$> Trans.liftIO (dlopen lib)
  State.modify
    (\env@Env {envLibs = libs} ->
        env
        { envLibs = handle : libs
        })

runExecute :: Env -> Execute a -> IO (Env, Maybe Value)
runExecute env m = do
  (val, env') <- runStateT (runExceptT m) env
  pure (env', either id (const Nothing) val)

declareVariable :: VarDecl -> Execute ()
declareVariable (VarDecl vtype vname) = do
  env <- State.get
  let Just env' = addVariableToEnv env vname vtype
  State.put env'

declareFunction :: FunctionDecl -> Execute ()
declareFunction (FunctionDecl rettype name params) = do
  env <- State.get
  let Just env' = addFunctionToEnv env name (Function rettype params Nothing)
  State.put env'

defineFunction :: FunctionDecl -> [Statement] -> Execute ()
defineFunction (FunctionDecl rettype name params) body = do
  env <- State.get
  lid <- currentLayer
  let Just env' =
        addFunctionToEnv
          env
          name
          (Function rettype params (Just $ FunctionBody (lid, body)))
  State.put env'

declareForeignFunction :: FunctionDecl -> Execute ()
declareForeignFunction (FunctionDecl rettype name@(FunctionName strname) params) = do
  env@Env {envLibs = libs} <- State.get
  Just f <- Trans.liftIO $ findSymbol libs strname
  let Just env' =
        addFunctionToEnv
          env
          name
          (Function rettype params (Just $ FunctionForeign f))
  State.put env'

printCall :: [Value] -> Execute ()
printCall vals = Trans.liftIO $ putStr $ concatMap show vals

dlopenCall :: [Value] -> Execute ()
dlopenCall vals =
  forM_ vals $
  \case
    (ValueString s) -> openLibrary s
    _ -> error "Type mismatch"

nativeFunctionCall
  :: Maybe VarType
  -> [VarDecl]
  -> [Value]
  -> LayerID
  -> [Statement]
  -> Execute (Maybe Value)
nativeFunctionCall rettype params vals lid body = do
  let varassignments = generateAssignments params vals
  let newbody = StatementBlock $ varassignments ++ body
  res <- withLayer lid $ executeStatementWithReturn newbody
  case (res, rettype) of
    (Nothing, Nothing) -> pure res
    (Just val, Just valtype) -> pure $ Just $ convert val valtype
    _ -> error "Type mismatch"

foreignFunctionCall :: Maybe VarType
                    -> [VarDecl]
                    -> [Value]
                    -> ForeignFun
                    -> Execute (Maybe Value)
foreignFunctionCall rettype params vals fun =
  Trans.liftIO $ call fun rettype (convertVals params vals)
  where
    convertVals [] [] = []
    convertVals (VarDecl vtype _:ps) (v:vs) = convert v vtype : convertVals ps vs
    convertVals _ _ = error "Type mismatch"

functionCall :: FunctionCall -> Execute (Maybe Value)
functionCall (FunctionCall (FunctionName "print") args) = do
  vals <- evaluateArgs args
  printCall vals
  pure Nothing
functionCall (FunctionCall (FunctionName "dlopen") args) = do
  vals <- evaluateArgs args
  dlopenCall vals
  pure Nothing
functionCall (FunctionCall fname args) = do
  env <- State.get
  vals <- evaluateArgs args
  let Just (Function rettype params (Just impl)) = getFunctionFromEnv env fname
  case impl of
    FunctionBody (lid, body) -> nativeFunctionCall rettype params vals lid body
    FunctionForeign f -> foreignFunctionCall rettype params vals f

generateAssignment :: VarDecl -> Value -> [Statement]
generateAssignment decl@(VarDecl _ name) val =
  [StatementVarDecl decl, StatementAssign name (valueToExpr val)]

generateAssignments :: [VarDecl] -> [Value] -> [Statement]
generateAssignments [] [] = []
generateAssignments (decl:decls) (val:vals) =
  generateAssignment decl val ++ generateAssignments decls vals
generateAssignments _ _ = error "Type mismatch"

evaluateArgs :: [Expr] -> Execute [Value]
evaluateArgs = mapM evaluate

executeStatementWithReturn :: Statement -> Execute (Maybe Value)
executeStatementWithReturn s = do
  lid <- currentLayer
  (execute s >> pure Nothing) `Except.catchError` restoreFromReturn lid
  where
    restoreFromReturn lid val = do
      _ <- splitLayers lid
      pure val

functionReturn :: Maybe Value -> Execute ()
functionReturn = Except.throwError

readVariable :: VarName -> Execute Value
readVariable name = do
  env <- State.get
  let Just val = readVariableFromEnv env name
  pure val

writeVariable :: VarName -> Value -> Execute ()
writeVariable name val = do
  env <- State.get
  let Just env' = writeVariableToEnv env name val
  State.put env'

evaluate :: Expr -> Execute Value
evaluate (ExprFunctionCall fcall) = do
  Just val <- functionCall fcall
  pure val
evaluate (ExprVar vname) = readVariable vname
evaluate (ExprInt i) = pure $ ValueInt i
evaluate (ExprFloat f) = pure $ ValueFloat f
evaluate (ExprString s) = pure $ ValueString s
evaluate (ExprNeg e) = negate <$> evaluate e
evaluate (ExprPlus el er) = (+) <$> evaluate el <*> evaluate er
evaluate (ExprMinus el er) = (-) <$> evaluate el <*> evaluate er
evaluate (ExprTimes el er) = (*) <$> evaluate el <*> evaluate er
evaluate (ExprDiv el er) = (/) <$> evaluate el <*> evaluate er
evaluate (ExprMod el er) = rem <$> evaluate el <*> evaluate er
evaluate (ExprNot e) = do
  val <- evaluate e
  case val of
    ValueInt 0 -> pure $ ValueInt 1
    ValueInt _ -> pure $ ValueInt 0
    _ -> error "Type mismatch"
evaluate (ExprAnd el er) =
  (\lhs rhs -> fromBool (toBool lhs && toBool rhs)) <$> evaluate el <*>
  evaluate er
evaluate (ExprOr el er) =
  (\lhs rhs -> fromBool (toBool lhs || toBool rhs)) <$> evaluate el <*>
  evaluate er
evaluate (ExprEq el er) = ((fromBool .) . (==)) <$> evaluate el <*> evaluate er
evaluate (ExprNeq el er) = ((fromBool .) . (/=)) <$> evaluate el <*> evaluate er
evaluate (ExprLt el er) = ((fromBool .) . (<)) <$> evaluate el <*> evaluate er
evaluate (ExprLeq el er) = ((fromBool .) . (<=)) <$> evaluate el <*> evaluate er
evaluate (ExprGt el er) = ((fromBool .) . (>)) <$> evaluate el <*> evaluate er
evaluate (ExprGeq el er) = ((fromBool .) . (>=)) <$> evaluate el <*> evaluate er

evaluateAsBool :: Expr -> Execute Bool
evaluateAsBool e = do
  i <- evaluateAsInt e
  pure $ i /= 0

evaluateAsInt :: Expr -> Execute Int
evaluateAsInt e = do
  ValueInt i <- evaluate e
  pure i

execute :: Statement -> Execute ()
execute (StatementBlock stmts) = withNewLayer (forM_ stmts execute)
execute (StatementFunctionCall fcall) = functionCall fcall >> pure ()
execute s@(StatementWhile e stmt) = do
  res <- evaluateAsBool e
  when res $
    do execute stmt
       execute s
execute (StatementVarDecl varDecl) = declareVariable varDecl
execute (StatementFunctionDecl funDecl) = declareFunction funDecl
execute (StatementForeignFunctionDecl funDecl) = declareForeignFunction funDecl
execute (StatementAssign var e) = do
  res <- evaluate e
  writeVariable var res
execute (StatementAssignPlus var e) =
  execute (StatementAssign var (ExprPlus (ExprVar var) e))
execute (StatementAssignMinus var e) =
  execute (StatementAssign var (ExprMinus (ExprVar var) e))
execute (StatementIfElse e strue sfalse) = do
  res <- evaluateAsBool e
  if res
    then execute strue
    else execute sfalse
execute (StatementIf e s) = do
  res <- evaluateAsBool e
  when res $ execute s
execute (StatementFor v e1 e2 s) = do
  i1 <- evaluateAsInt e1
  i2 <- evaluateAsInt e2
  forM_ [i1 .. i2] $
    \i -> do
      writeVariable v $ ValueInt i
      execute s
execute (StatementFunctionDef funDecl stmts) = defineFunction funDecl stmts
execute (StatementReturn Nothing) = functionReturn Nothing
execute (StatementReturn (Just e)) = do
  res <- evaluate e
  functionReturn (Just res)
