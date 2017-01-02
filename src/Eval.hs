{-# LANGUAGE LambdaCase #-}

module Eval
  ( eval
  ) where

import Control.Monad (forM, forM_, when)
import Control.Monad.State (StateT, runStateT)
import qualified Control.Monad.State as State
import Control.Monad.Except (ExceptT, runExceptT)
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Trans as Trans
import Data.Bits
import Data.Foldable (asum)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.List as List
import Data.Maybe

import ForeignEval
import Syntax
import Value

eval :: Program -> IO ()
eval p = do
  libhandles <-
    forM (programLibraries p) $
    \l -> do
      Right h <- dlopen l
      pure h
  runExecute $ startExecute (programForeignFunctions p) (programStatements p)
  forM_ libhandles dlclose
  pure ()

type LayerID = Int

data FunctionImpl
  = FunctionBody (LayerID, [VarID], Block)

data Function =
  Function (Maybe VarType)
           [VarType]
           (Maybe FunctionImpl)

functionTypesMatch :: Function -> Function -> Bool
functionTypesMatch (Function lrettype lparams _) (Function rrettype rparams _) =
  lrettype == rrettype && lparams == rparams

data Layer = Layer
  { varEnv :: IntMap Value
  , funEnv :: IntMap Function
  }

emptyLayer :: Layer
emptyLayer = Layer
  { varEnv = IntMap.empty
  , funEnv = IntMap.empty
  }

readVariableFromLayer :: Layer -> VarID -> Maybe Value
readVariableFromLayer l (VarID vid) = IntMap.lookup vid (varEnv l)

writeVariableToLayer :: Layer -> VarID -> Value -> Maybe Layer
writeVariableToLayer l (VarID vid) value =
  let (moldvalue, newvarenv) =
        IntMap.insertLookupWithKey
          (\_ newVal oldVal -> convert newVal (typeof oldVal))
          vid
          value
          (varEnv l)
  in case moldvalue of
       Nothing -> Nothing
       Just _ ->
         Just $
         l
         { varEnv = newvarenv
         }

addVariableToLayer :: Layer -> VarID -> VarType -> Maybe Layer
addVariableToLayer l (VarID vid) vtype =
  let (oldvalue, newvarenv) =
        IntMap.insertLookupWithKey
          (\_ a _ -> a)
          vid
          (defaultValueFromType vtype)
          (varEnv l)
  in maybe
       (Just $
        l
        { varEnv = newvarenv
        })
       (const Nothing)
       oldvalue

getFunctionFromLayer :: Layer -> FunID -> Maybe Function
getFunctionFromLayer l (FunID fid) = IntMap.lookup fid (funEnv l)

addFunctionToLayer :: Layer -> FunID -> Function -> Maybe Layer
addFunctionToLayer l (FunID fid) f@(Function _ _ body) =
  let (oldvalue, newfunenv) =
        IntMap.insertLookupWithKey (\_ a _ -> a) fid f (funEnv l)
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
  { envLayers :: [Layer]
  , envForeignFunctions :: IntMap ForeignFun
  }

emptyEnv :: Env
emptyEnv = Env
  { envLayers = []
  , envForeignFunctions = IntMap.empty
  }

modifyingLayer :: (Layer -> Maybe Layer) -> [Layer] -> Maybe [Layer]
modifyingLayer _ [] = Nothing
modifyingLayer f (l:ls) =
  case f l of
    Nothing -> (l :) <$> modifyingLayer f ls
    Just l' -> Just (l' : ls)

readVariableFromEnv :: Env -> VarID -> Maybe Value
readVariableFromEnv Env {envLayers = curlayer:otherlayers} vname =
  asum (map (`readVariableFromLayer` vname) (curlayer : otherlayers))
readVariableFromEnv _ _ = error "Env broke"

writeVariableToEnv :: Env -> VarID -> Value -> Maybe Env
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

addVariableToEnv :: Env -> VarID -> VarType -> Maybe Env
addVariableToEnv env@Env {envLayers = curlayer:otherlayers} vname vtype =
  case addVariableToLayer curlayer vname vtype of
    Nothing -> Nothing
    Just l' ->
      Just
        (env
         { envLayers = l' : otherlayers
         })
addVariableToEnv _ _ _ = error "Env broke"

getFunctionFromEnv :: Env -> FunID -> Maybe Function
getFunctionFromEnv Env {envLayers = curlayer:otherlayers} name =
  asum (map (`getFunctionFromLayer` name) (curlayer : otherlayers))
getFunctionFromEnv _ _ = error "Env broke"

addFunctionToEnv :: Env -> FunID -> Function -> Maybe Env
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

runExecute :: Execute () -> IO ()
runExecute m = do
  _ <- runStateT (runExceptT m) emptyEnv
  pure ()

startExecute :: IntMap String -> Block -> Execute ()
startExecute foreignFuns stmts = do
  mfuns <- mapM (Trans.liftIO . findSymbol) foreignFuns
  State.modify $ \env -> env { envForeignFunctions = fromJust <$> mfuns }
  execute (StatementBlock stmts)

declareVariable :: VarDecl -> Execute ()
declareVariable (VarDecl vtype vname) = do
  env <- State.get
  let Just env' = addVariableToEnv env vname vtype
  State.put env'

defineFunction :: FunctionDef -> Execute ()
defineFunction f = do
  env <- State.get
  lid <- currentLayer
  let (params, paramNames) = unzip $ map (\(VarDecl t n) -> (t, n)) $ funDefParams f
  let Just env' =
        addFunctionToEnv
          env
          (funDefName f)
          (Function (funDefRetType f) params (Just $ FunctionBody (lid, paramNames, funDefBody f)))
  State.put env'

printCall :: [Value] -> Execute ()
printCall vals =
  Trans.liftIO $
  do str <- concat <$> mapM showIO vals
     putStr str

nativeFunctionCall
  :: Maybe VarType
  -> [VarType]
  -> [VarID]
  -> [Value]
  -> LayerID
  -> Block
  -> Execute (Maybe Value)
nativeFunctionCall rettype params paramNames vals lid body = do
  res <-
    withLayer lid $
    withNewLayer $
    do generateAssignments (paramDecls params paramNames) vals
       executeBlockWithReturn body
  case (res, rettype) of
    (Nothing, Nothing) -> pure res
    (Just val, Just valtype) -> pure $ Just $ convert val valtype
    _ -> error "Type mismatch"
  where
    paramDecls [] [] = []
    paramDecls (t:ts) (n:ns) = VarDecl t n : paramDecls ts ns
    paramDecls _ _ = error "Type mismatch"

foreignFunctionCall :: Maybe VarType
                    -> [VarType]
                    -> [Value]
                    -> ForeignFun
                    -> Execute (Maybe Value)
foreignFunctionCall rettype params vals fun =
  Trans.liftIO $ call fun rettype (convertVals params vals)
  where
    convertVals [] [] = []
    convertVals (vtype:ps) (v:vs) = convert v vtype : convertVals ps vs
    convertVals _ _ = error "Type mismatch"

functionCall :: FunctionCall -> Execute (Maybe Value)
functionCall (PrintCall args) = do
  vals <- evaluateArgs args
  printCall vals
  pure Nothing
functionCall (ForeignFunctionCall fdecl args) = do
  vals <- evaluateArgs args
  let (FunID fid) = foreignFunDeclName fdecl
  Just f <- (IntMap.lookup fid . envForeignFunctions) <$> State.get
  foreignFunctionCall (foreignFunDeclRetType fdecl) (foreignFunDeclParams fdecl) vals f
functionCall (NativeFunctionCall fname args) = do
  env <- State.get
  vals <- evaluateArgs args
  let Just (Function rettype params (Just impl)) = getFunctionFromEnv env fname
  case impl of
    FunctionBody (lid, paramNames, body) -> nativeFunctionCall rettype params paramNames vals lid body

generateAssignment :: VarDecl -> Value -> Execute ()
generateAssignment decl@(VarDecl _ name) val = do
  declareVariable decl
  writeVariable name val

generateAssignments :: [VarDecl] -> [Value] -> Execute ()
generateAssignments [] [] = pure ()
generateAssignments (decl:decls) (val:vals) = do
  generateAssignment decl val
  generateAssignments decls vals
generateAssignments _ _ = error "Type mismatch"

evaluateArgs :: [Expr] -> Execute [Value]
evaluateArgs = mapM evaluate

executeBlockWithReturn :: Block -> Execute (Maybe Value)
executeBlockWithReturn block = do
  lid <- currentLayer
  (executeBlock block >> pure Nothing) `Except.catchError` restoreFromReturn lid
  where
    restoreFromReturn lid val = do
      _ <- splitLayers lid
      pure val

functionReturn :: Maybe Value -> Execute ()
functionReturn = Except.throwError

readVariable :: VarID -> Execute Value
readVariable name = do
  env <- State.get
  let Just val = readVariableFromEnv env name
  pure val

writeVariable :: VarID -> Value -> Execute ()
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
evaluate (ExprString s) = pure $ ValueString $ Right s
evaluate (ExprNeg e) = negate <$> evaluate e
evaluate (ExprPlus el er) = (+) <$> evaluate el <*> evaluate er
evaluate (ExprMinus el er) = (-) <$> evaluate el <*> evaluate er
evaluate (ExprTimes el er) = (*) <$> evaluate el <*> evaluate er
evaluate (ExprDiv el er) = (/) <$> evaluate el <*> evaluate er
evaluate (ExprMod el er) = rem <$> evaluate el <*> evaluate er
evaluate (ExprBitAnd el er) = (.&.) <$> evaluate el <*> evaluate er
evaluate (ExprBitOr el er) = (.|.) <$> evaluate el <*> evaluate er
evaluate (ExprBitXor el er) = xor <$> evaluate el <*> evaluate er
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
evaluate (ExprLt el er) = ((fromBool .) . (<)) <$> evaluate el <*> evaluate er

evaluateAsBool :: Expr -> Execute Bool
evaluateAsBool e = do
  i <- evaluateAsInt e
  pure $ i /= 0

evaluateAsInt :: Expr -> Execute Int
evaluateAsInt e = do
  ValueInt i <- evaluate e
  pure i

executeBlock :: Block -> Execute ()
executeBlock block = withNewLayer $ do
  forM_ (blockVariables block) declareVariable
  forM_ (blockFunctions block) defineFunction
  forM_ (blockStatements block) execute

execute :: Statement -> Execute ()
execute StatementNoop = pure ()
execute (StatementBlock block) = executeBlock block
execute (StatementFunctionCall fcall) = functionCall fcall >> pure ()
execute s@(StatementWhile e block) = do
  res <- evaluateAsBool e
  when res $
    do executeBlock block
       execute s
execute (StatementAssign var e) = do
  res <- evaluate e
  writeVariable var res
execute (StatementIfElse e btrue bfalse) = do
  res <- evaluateAsBool e
  if res
    then executeBlock btrue
    else executeBlock bfalse
execute (StatementReturn Nothing) = functionReturn Nothing
execute (StatementReturn (Just e)) = do
  res <- evaluate e
  functionReturn (Just res)
