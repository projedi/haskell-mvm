module BytecodeInterpreter
  ( interpret
  ) where

import Control.Monad
import Control.Monad.Except (ExceptT, runExceptT)
import qualified Control.Monad.Except as Except
import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Reader as Reader
import Control.Monad.State (StateT, execStateT)
import qualified Control.Monad.State as State
import qualified Control.Monad.Trans as Trans
import Data.Bits
import Data.Foldable (asum)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Bytecode
import ForeignEval
import Syntax (VarType(..))
import Value

interpret :: Bytecode -> IO ()
interpret bc = do
  libhandles <-
    forM (bytecodeLibraries bc) $
    \l -> do
      Right h <- dlopen l
      pure h
  _ <- runInterpreter startInterpretation bc emptyEnv
  forM_ libhandles dlclose
  pure ()

data Layer = Layer
  { vars :: IntMap Value
  }

emptyLayer :: Layer
emptyLayer = Layer IntMap.empty

introduceVariableToLayer :: Layer -> VarID -> VarType -> Maybe Layer
introduceVariableToLayer l (VarID v) vtype =
  case IntMap.insertLookupWithKey f v (defaultValueFromType vtype) (vars l) of
    (Nothing, vs) ->
      Just $
      l
      { vars = vs
      }
    (Just _, vs) ->
      Just $
      l
      { vars = vs
      }
  where
    f _ newVal oldVal
      | typeof newVal == typeof oldVal = oldVal
      | otherwise = error "Type mismatch"

writeVariableToLayer :: Layer -> VarID -> Value -> Maybe Layer
writeVariableToLayer l (VarID v) val =
  case IntMap.insertLookupWithKey f v val (vars l) of
    (Nothing, _) -> Nothing
    (Just _, vs) ->
      Just $
      l
      { vars = vs
      }
  where
    f _ newVal oldVal
      | typeof newVal == typeof oldVal = newVal
      | otherwise = error "Type mismatch"

readVariableFromLayer :: Layer -> VarID -> Maybe Value
readVariableFromLayer (Layer vs) (VarID v) = IntMap.lookup v vs

data Pos =
  Pos FunID
      Int

nextPos :: Pos -> Pos
nextPos (Pos f l) = Pos f (l + 1)

data ConstEnv = ConstEnv
  { labels :: IntMap Pos
  , bytecode :: Bytecode
  }

resolveLabelInOp :: Pos -> Op -> IntMap Pos
resolveLabelInOp p (OpLabel (LabelID l)) = IntMap.singleton l p
resolveLabelInOp _ _ = IntMap.empty

resolveLabelsInFun :: FunID -> BytecodeFunction -> IntMap Pos
resolveLabelsInFun f (BytecodeFunction ops) =
  mconcat $ map (\(l, op) -> resolveLabelInOp (Pos f l) op) $ zip [0 ..] ops

resolveLabels :: Bytecode -> IntMap Pos
resolveLabels (Bytecode {bytecodeFunctions = funs}) =
  mconcat $ map (\(k, v) -> resolveLabelsInFun (FunID k) v) $ IntMap.toList funs

createConstEnv :: Bytecode -> ConstEnv
createConstEnv bc =
  ConstEnv
  { labels = resolveLabels bc
  , bytecode = bc
  }

data Env = Env
  { layers :: [Layer]
  , stack :: [Value]
  , pos :: Pos
  }

emptyEnv :: Env
emptyEnv =
  Env
  { layers = []
  , stack = []
  , pos = Pos (FunID 0) 0
  }

modifyingLayer :: (Layer -> Maybe Layer) -> [Layer] -> Maybe [Layer]
modifyingLayer _ [] = Nothing
modifyingLayer f (l:ls) =
  case f l of
    Nothing -> (l :) <$> modifyingLayer f ls
    Just l' -> Just (l' : ls)

introduceVariableInEnv :: Env -> VarID -> VarType -> Env
introduceVariableInEnv env@Env {layers = l:ls} v vtype =
  case introduceVariableToLayer l v vtype of
    Nothing -> error "Type mismatch"
    Just l' ->
      env
      { layers = l' : ls
      }
introduceVariableInEnv _ _ _ = error "Env broke"

writeVariableToEnv :: Env -> VarID -> Value -> Env
writeVariableToEnv env@Env {layers = ls} v val =
  case modifyingLayer (\l -> writeVariableToLayer l v val) ls of
    Just ls' ->
      env
      { layers = ls'
      }
    Nothing -> error "Type mismatch"

readVariableFromEnv :: Env -> VarID -> Value
readVariableFromEnv Env {layers = ls} v =
  let Just val = asum (map (`readVariableFromLayer` v) ls)
  in val

jumpToLabelInEnv :: ConstEnv -> Env -> LabelID -> Env
jumpToLabelInEnv cenv env (LabelID l) =
  case IntMap.lookup l (labels cenv) of
    Nothing -> error "Invalid label"
    Just p ->
      env
      { pos = p
      }

type Interpreter a = ExceptT () (StateT Env (ReaderT ConstEnv IO)) a

runInterpreter :: Interpreter a -> Bytecode -> Env -> IO Env
runInterpreter m bc env =
  runReaderT (execStateT (runExceptT m) env) (createConstEnv bc)

pop :: Interpreter Value
pop = do
  v:vs <- stack <$> State.get
  State.modify $
    \env ->
       env
       { stack = vs
       }
  pure v

push :: Value -> Interpreter ()
push v =
  State.modify $
  \env ->
     env
     { stack = v : stack env
     }

startInterpretation :: Interpreter ()
startInterpretation = interpretFunction (FunID 0)

interpretFunction :: FunID -> Interpreter ()
interpretFunction f = do
  posBefore <- pos <$> State.get
  State.modify $
    \env ->
       env
       { pos = Pos f 0
       , layers = emptyLayer : layers env
       }
  interpretationLoop `Except.catchError` pure
  State.modify $
    \env ->
       env
       { pos = posBefore
       , layers = tail $ layers env
       }

getCurrentOp :: Interpreter (Maybe Op)
getCurrentOp = do
  funs <- (bytecodeFunctions . bytecode) <$> Reader.ask
  Pos (FunID f) l <- pos <$> State.get
  let Just (BytecodeFunction ops) = IntMap.lookup f funs
  if l >= length ops
    then pure Nothing
    else pure $ Just $ ops !! l

advancePos :: Interpreter ()
advancePos =
  State.modify $
  \env ->
     env
     { pos = nextPos $ pos env
     }

interpretationLoop :: Interpreter ()
interpretationLoop = do
  mop <- getCurrentOp
  case mop of
    Nothing -> pure () -- We've reached the end
    Just op -> do
      interpretOp op
      advancePos -- Will work even after jump: advances to next op after label.
      interpretationLoop

introduceVariable :: VarID -> VarType -> Interpreter ()
introduceVariable v vtype =
  State.modify $ \env -> introduceVariableInEnv env v vtype

performReturn :: Interpreter ()
performReturn = Except.throwError ()

findForeignFunction :: String -> Interpreter ForeignFun
findForeignFunction fname = do
  Just f <- Trans.liftIO $ findSymbol fname
  pure f

foreignFunctionCall :: String -> Maybe VarType -> [VarType] -> Interpreter ()
foreignFunctionCall fname rettype argtypes = do
  fun <- findForeignFunction fname
  vals <-
    forM argtypes $
    \expectedType -> do
      v <- pop
      when (typeof v /= expectedType) $ error "Type mismatch"
      pure v
  retVal <- Trans.liftIO $ call fun rettype vals
  case retVal of
    Nothing -> pure ()
    Just v -> push v

pushConstant :: ConstID -> Interpreter ()
pushConstant (ConstID cid) = do
  consts <- (bytecodeConstants . bytecode) <$> Reader.ask
  let Just val = IntMap.lookup cid consts
  push val

jump :: LabelID -> Interpreter ()
jump l = do
  cEnv <- Reader.ask
  State.modify $ \env -> jumpToLabelInEnv cEnv env l

store :: VarID -> Interpreter ()
store v = do
  val <- pop
  State.modify $ \env -> writeVariableToEnv env v val

load :: VarID -> Interpreter ()
load v = do
  env <- State.get
  let val = readVariableFromEnv env v
  push val

interpretUnaryOp :: (Value -> Value) -> Interpreter ()
interpretUnaryOp f = do
  v <- pop
  push $ f v

interpretBinOp :: (Value -> Value -> Value) -> Interpreter ()
interpretBinOp f = do
  lhs <- pop
  rhs <- pop
  push $ f lhs rhs

interpretCompOp :: (Value -> Value -> Bool) -> Interpreter ()
interpretCompOp f = interpretBinOp ((fromBool .) . f)

interpretOp :: Op -> Interpreter ()
interpretOp (OpCall f) = interpretFunction f
interpretOp (OpIntroVar v vtype) = introduceVariable v vtype
interpretOp OpReturn = performReturn
interpretOp (OpForeignCall f rettype argtypes) = foreignFunctionCall f rettype argtypes
interpretOp (OpLabel _) = pure () -- Resolved already
interpretOp (OpJump l) = jump l
interpretOp (OpJumpIfZero l) = do
  ValueInt i <- pop
  if i == 0
    then jump l
    else pure ()
interpretOp (OpPushInt cid) = pushConstant cid
interpretOp (OpPushFloat cid) = pushConstant cid
interpretOp (OpPushString cid) = pushConstant cid
interpretOp OpPop = pop >> pure ()
interpretOp (OpStore v) = store v
interpretOp (OpLoad v) = load v
interpretOp OpNegateInt = interpretUnaryOp negate
interpretOp OpNegateFloat = interpretUnaryOp negate
interpretOp OpPlusInt = interpretBinOp (+)
interpretOp OpPlusFloat = interpretBinOp (+)
interpretOp OpMinusInt = interpretBinOp (-)
interpretOp OpMinusFloat = interpretBinOp (-)
interpretOp OpTimesInt = interpretBinOp (*)
interpretOp OpTimesFloat = interpretBinOp (*)
interpretOp OpDivInt = interpretBinOp (/)
interpretOp OpDivFloat = interpretBinOp (/)
interpretOp OpModInt = interpretBinOp rem
interpretOp OpBitAndInt = interpretBinOp (.&.)
interpretOp OpBitOrInt = interpretBinOp (.|.)
interpretOp OpBitXorInt = interpretBinOp xor
interpretOp OpNotInt = interpretUnaryOp (fromBool . not . toBool)
interpretOp OpAndInt =
  interpretBinOp (\lhs rhs -> fromBool (toBool lhs && toBool rhs))
interpretOp OpOrInt =
  interpretBinOp (\lhs rhs -> fromBool (toBool lhs || toBool rhs))
interpretOp OpEqInt = interpretCompOp (==)
interpretOp OpEqFloat = interpretCompOp (==)
interpretOp OpLtInt = interpretCompOp (<)
interpretOp OpLtFloat = interpretCompOp (<)
interpretOp OpIntToFloat = interpretUnaryOp (`convert` VarTypeFloat)
