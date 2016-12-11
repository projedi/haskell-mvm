module Eval (eval) where

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
import qualified Numeric

import Syntax

eval :: Program -> IO ()
eval (Program stmts) = do
  _ <- runExecute emptyEnv (execute (StatementBlock stmts))
  pure ()

data Value
  = ValueInt Int
  | ValueFloat Double
  | ValueString String

typeIs :: Value -> VarType -> Bool
typeIs v vtype = typeof v == vtype

typeof :: Value -> VarType
typeof (ValueInt _) = VarTypeInt
typeof (ValueFloat _) = VarTypeFloat
typeof (ValueString _) = VarTypeString

valueToExpr :: Value -> Expr
valueToExpr (ValueInt i) = ExprInt i
valueToExpr (ValueFloat f) = ExprFloat f
valueToExpr (ValueString s) = ExprString s

printValue :: Value -> String
printValue (ValueInt i) = show i
printValue (ValueFloat f)
 | stripedAfter == "." = before
 | otherwise = before ++ stripedAfter
 where
  str = Numeric.showFFloat (Just 3) f ""
  (before, after) = List.span (/= '.') str
  stripedAfter = List.dropWhileEnd (== '0') after
printValue (ValueString s) = s

instance Eq Value where
  (ValueInt il) == (ValueInt ir) = il == ir
  (ValueFloat fl) == (ValueFloat fr) = fl == fr
  (ValueString sl) == (ValueString sr) = sl == sr
  _ == _ = error "Type mismatch"

instance Ord Value where
  compare (ValueInt il) (ValueInt ir) = compare il ir
  compare (ValueFloat fl) (ValueFloat fr) = compare fl fr
  compare (ValueString sl) (ValueString sr) = compare sl sr
  compare _ _ = error "Type mismatch"

instance Num Value where
  (ValueInt il) + (ValueInt ir) = ValueInt (il + ir)
  (ValueInt il) + (ValueFloat fr) = ValueFloat (fromIntegral il + fr)
  (ValueFloat fl) + (ValueInt ir) = ValueFloat (fl + fromIntegral ir)
  (ValueFloat fl) + (ValueFloat fr) = ValueFloat (fl + fr)
  (ValueString sl) + (ValueString sr) = ValueString (sl ++ sr)
  _ + _ = error "Type mismatch"

  (ValueInt il) * (ValueInt ir) = ValueInt (il * ir)
  (ValueInt il) * (ValueFloat fr) = ValueFloat (fromIntegral il * fr)
  (ValueFloat fl) * (ValueInt ir) = ValueFloat (fl * fromIntegral ir)
  (ValueFloat fl) * (ValueFloat fr) = ValueFloat (fl * fr)
  _ * _ = error "Type mismatch"

  fromInteger i = ValueInt (fromInteger i)

  signum (ValueInt i) = ValueInt (signum i)
  signum (ValueFloat f) = ValueFloat (signum f)
  signum _ = error "Type mismatch"

  abs (ValueInt i) = ValueInt (abs i)
  abs (ValueFloat f) = ValueFloat (abs f)
  abs _ = error "Type mismatch"

  negate (ValueInt i) = ValueInt (negate i)
  negate (ValueFloat f) = ValueFloat (negate f)
  negate _ = error "Type mismatch"

instance Enum Value where
  toEnum i = ValueInt (toEnum i)

  fromEnum (ValueInt i) = fromEnum i
  fromEnum _ = error "Type mismatch"

instance Real Value where
  toRational (ValueInt i) = toRational i
  toRational (ValueFloat f) = toRational f
  toRational _ = error "Type mismatch"

instance Integral Value where
  toInteger (ValueInt i) = toInteger i
  toInteger _ = error "Type mismatch"

  quotRem (ValueInt il) (ValueInt ir) =
    let (rl, rr) = quotRem il ir
    in (ValueInt rl, ValueInt rr)
  quotRem _ _ = error "Type mismatch"

instance Fractional Value where
  fromRational r = ValueFloat (fromRational r)
  (ValueInt il) / (ValueInt ir) = ValueInt (il `div` ir)
  (ValueFloat fl) / (ValueInt ir) = ValueFloat (fl / fromIntegral ir)
  (ValueInt il) / (ValueFloat fr) = ValueFloat (fromIntegral il / fr)
  (ValueFloat fl) / (ValueFloat fr) = ValueFloat (fl / fr)
  _ / _ = error "Type mismatch"

fromBool :: Bool -> Value
fromBool True = ValueInt 1
fromBool False = ValueInt 0

defaultValueFromType :: VarType -> Value
defaultValueFromType VarTypeInt = ValueInt 0
defaultValueFromType VarTypeFloat = ValueFloat 0
defaultValueFromType VarTypeString = ValueString ""

data Function = Function (Maybe VarType) [VarDecl] (Maybe [Statement])

argsMatch :: [VarDecl] -> [VarDecl] -> Bool
argsMatch largs rargs = map (\(VarDecl t _) -> t) largs == map (\(VarDecl t _) -> t) rargs

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
  let (moldvalue, newvarenv) = Map.insertLookupWithKey (\_ a _ -> a) name value (varEnv l)
  in case moldvalue of
       Nothing -> Nothing
       Just oldvalue
        | typeof oldvalue == typeof value -> Just $ l { varEnv = newvarenv }
        | otherwise -> error "Type mismatch"

addVariableToLayer :: Layer -> VarName -> VarType -> Maybe Layer
addVariableToLayer l name vtype =
  let (oldvalue, newvarenv) = Map.insertLookupWithKey (\_ a _ -> a) name (defaultValueFromType vtype) (varEnv l)
  in maybe (Just $ l { varEnv = newvarenv }) (const $ Nothing) oldvalue

getFunctionFromLayer :: Layer -> FunctionName -> Maybe Function
getFunctionFromLayer l name = Map.lookup name (funEnv l)

addFunctionToLayer :: Layer -> FunctionName -> Function -> Maybe Layer
addFunctionToLayer l name f@(Function _ _ body) =
  let (oldvalue, newfunenv) = Map.insertLookupWithKey (\_ a _ -> a) name f (funEnv l)
  in case (oldvalue, body) of
       (Nothing, _) -> Just $ l { funEnv = newfunenv }
       (Just (Function _ _ (Just _)), Just _) -> Nothing
       (Just oldf@(Function _ _ (Just _)), Nothing)
        | functionTypesMatch oldf f -> Just l
        | otherwise -> Nothing
       (Just oldf@(Function _ _ Nothing), _)
        | functionTypesMatch oldf f -> Just $ l { funEnv = newfunenv }
        | otherwise -> Nothing

type Env = (Layer, [Layer])

emptyEnv :: Env
emptyEnv = (emptyLayer, [])

modifyingLayer :: (Layer -> Maybe Layer) -> [Layer] -> Maybe [Layer]
modifyingLayer _ [] = Nothing
modifyingLayer f (l:ls) =
  case f l of
    Nothing -> (l:) <$> modifyingLayer f ls
    Just l' -> Just (l':ls)

readVariableFromEnv :: Env -> VarName -> Maybe Value
readVariableFromEnv (curlayer, otherlayers) vname =
  asum (map (\l -> readVariableFromLayer l vname) (curlayer : otherlayers))

writeVariableToEnv :: Env -> VarName -> Value -> Maybe Env
writeVariableToEnv (curlayer, otherlayers) vname val =
  case (modifyingLayer (\l -> writeVariableToLayer l vname val) (curlayer : otherlayers)) of
    Just (curlayer' : otherlayers') -> Just (curlayer', otherlayers')
    Just _ -> error "Impossible"
    Nothing -> Nothing

addVariableToEnv :: Env -> VarName -> VarType -> Maybe Env
addVariableToEnv (curlayer, otherlayers) vname vtype =
  case addVariableToLayer curlayer vname vtype of
    Nothing -> Nothing
    Just l' -> Just (l', otherlayers)

getFunctionFromEnv :: Env -> FunctionName -> Maybe Function
getFunctionFromEnv (curlayer, otherlayers) name =
  asum (map (\l -> getFunctionFromLayer l name) (curlayer : otherlayers))

addFunctionToEnv :: Env -> FunctionName -> Function -> Maybe Env
addFunctionToEnv (curlayer, otherlayers) fname f =
  case addFunctionToLayer curlayer fname f of
    Nothing -> Nothing
    Just l' -> Just (l', otherlayers)

type Execute a = ExceptT (Maybe Value) (StateT Env IO) a

newLayer :: Execute ()
newLayer = do
  (l, ls) <- State.get
  State.put (emptyLayer, l : ls)

dropLayer :: Execute ()
dropLayer = do
  (_, l : ls) <- State.get
  State.put (l, ls)

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
  let Just env' = addFunctionToEnv env name (Function rettype params (Just body))
  State.put env'

printCall :: [Value] -> Execute ()
printCall vals =
  Trans.liftIO $ putStrLn $ List.intercalate " " (map printValue vals)

functionCall :: FunctionCall -> Execute (Maybe Value)
functionCall (FunctionCall (FunctionName "print") args) = do
  vals <- evaluateArgs args
  printCall vals
  pure Nothing
functionCall (FunctionCall fname args) = do
  env <- State.get
  let Just (Function rettype params (Just body)) = getFunctionFromEnv env fname
  vals <- evaluateArgs args
  let varassignments = generateAssignments params vals
  let newbody = StatementBlock $ varassignments ++ body
  res <- executeStatementWithReturn newbody
  case (res, rettype) of
    (Nothing, Nothing) -> pure res
    (Just val, Just valtype)
     | val `typeIs` valtype -> pure res
     | otherwise -> error "Type mismatch"
    _ -> error "Type mismatch"

generateAssignment :: VarDecl -> Value -> [Statement]
generateAssignment decl@(VarDecl _ name) val =
  [ StatementVarDecl decl
  , StatementAssign name (valueToExpr val)
  ]

generateAssignments :: [VarDecl] -> [Value] -> [Statement]
generateAssignments [] [] = []
generateAssignments (decl:decls) (val:vals) = generateAssignment decl val ++ generateAssignments decls vals
generateAssignments _ _ = error "Type mismatch"

evaluateArgs :: [Expr] -> Execute [Value]
evaluateArgs = mapM evaluate

executeStatementWithReturn :: Statement -> Execute (Maybe Value)
executeStatementWithReturn s =
  (execute s >> pure Nothing) `Except.catchError` pure

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
evaluate (ExprEq el er) = ((fromBool .) . (==)) <$> evaluate el <*> evaluate er
evaluate (ExprNeq el er) = ((fromBool .) . (/=)) <$> evaluate el <*> evaluate er
evaluate (ExprLt el er) = ((fromBool .) . (<)) <$> evaluate el <*> evaluate er
evaluate (ExprLeq el er) = ((fromBool .) . (<=)) <$> evaluate el <*> evaluate er
evaluate (ExprGt el er) = ((fromBool .) . (>) )<$> evaluate el <*> evaluate er
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
execute (StatementBlock stmts) = do
  newLayer
  forM_ stmts execute
  dropLayer
execute (StatementFunctionCall fcall) = functionCall fcall >> pure ()
execute s@(StatementWhile e stmt) = do
  res <- evaluateAsBool e
  when res $ do
    execute stmt
    execute s
execute (StatementVarDecl varDecl) = declareVariable varDecl
execute (StatementFunctionDecl funDecl) = declareFunction funDecl
execute (StatementAssign var e) = do
  res <- evaluate e
  writeVariable var res
execute (StatementIfElse e strue sfalse) = do
  res <- evaluateAsBool e
  if res then execute strue else execute sfalse
execute (StatementIf e s) = do
  res <- evaluateAsBool e
  when res $ do
    execute s
execute (StatementFor v e1 e2 s) = do
  i1 <- evaluateAsInt e1
  i2 <- evaluateAsInt e2
  forM_ [i1..i2] $ \i -> do
    writeVariable v $ ValueInt i
    execute s
execute (StatementFunctionDef funDecl stmts) = defineFunction funDecl stmts
execute (StatementReturn Nothing) = functionReturn Nothing
execute (StatementReturn (Just e)) = do
  res <- evaluate e
  functionReturn (Just res)
