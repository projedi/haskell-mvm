module BytecodeTranslator
  ( translate
  ) where

import Control.Monad
import Control.Monad.State (State)
import qualified Control.Monad.State as State
import qualified Control.Monad.Trans as Trans
import Control.Monad.Writer (WriterT)
import qualified Control.Monad.Writer as Writer
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Bytecode
import Syntax
import Value (Value(..))

translate :: Program -> Bytecode
translate p =
  Bytecode
  { bytecodeLibraries = programLibraries p
  , bytecodeConstants = consts finalEnv
  , bytecodeForeignFunctions = foreignFuns finalEnv
  , bytecodeFunctions = bcFuns
  }
  where
    initEnv =
      emptyEnv
      { lastForeignFun = programLastFunID p
      , foreignFuns = programForeignFunctions p
      , lastVar = programLastVarID p
      }
    (bcFuns, finalEnv) = runTranslator (programFunctions p) initEnv

data Env = Env
  { lastLabel :: LabelID
  , lastConst :: ConstID
  , consts :: IntMap Value
  , foreignFuns :: IntMap ForeignFunctionDecl
  , lastForeignFun :: FunID
  , captureMap :: IntMap VarID
  , lastVar :: VarID
  }

emptyEnv :: Env
emptyEnv =
  Env
  { lastLabel = LabelID (-1)
  , lastConst = ConstID (-1)
  , consts = IntMap.empty
  , foreignFuns = IntMap.empty
  , lastForeignFun = FunID 0
  , captureMap = IntMap.empty
  , lastVar = VarID 0
  }

type Translator a = WriterT BytecodeFunction (State Env) a

runTranslator :: IntMap FunctionDef -> Env -> (IntMap BytecodeFunction, Env)
runTranslator fs = State.runState (fst <$> Writer.runWriterT (mapM getBCFun fs))
  where
    getBCFun f = snd <$> localTranslate (translateFunctionBody f)

newLabel :: Translator LabelID
newLabel = do
  label <- (inc . lastLabel) <$> State.get
  State.modify $
    \env ->
       env
       { lastLabel = label
       }
  pure label
  where
    inc (LabelID lbl) = LabelID (lbl + 1)

addOp :: Op -> Translator ()
addOp op = Writer.tell (BytecodeFunction [op])

introduceVariable :: VarDecl -> Translator VarID
introduceVariable (VarDecl vtype v) = do
  addOp $ OpIntroVar v vtype
  pure v

newVariable :: VarType -> Translator VarID
newVariable vtype = do
  v <- (inc . lastVar) <$> State.get
  State.modify $
    \env ->
       env
       { lastVar = v
       }
  introduceVariable (VarDecl vtype v)
  where
    inc (VarID vid) = VarID (vid + 1)

loadVar :: VarID -> Translator ()
loadVar (VarID vid) = do
  realVar <- (IntMap.lookup vid . captureMap) <$> State.get
  case realVar of
    Nothing -> addOp $ OpLoad (VarID vid)
    Just v -> addOp $ OpLoadPtr v

storeVar :: VarID -> Translator ()
storeVar (VarID vid) = do
  realVar <- (IntMap.lookup vid . captureMap) <$> State.get
  case realVar of
    Nothing -> addOp $ OpStore (VarID vid)
    Just v -> addOp $ OpStorePtr v

translateFunctionBody :: FunctionDef -> Translator ()
translateFunctionBody f = do
  vs <- mapM introduceVariable $ funDefParams f
  forM_ vs (addOp . OpStore)
  translateCapturesInBody $ funDefCaptures f
  translateStatement $ StatementBlock $ funDefBody f

translateBlock :: Block -> Translator ()
translateBlock block = do
  forM_ (blockVariables block) introduceVariable
  forM_ (blockStatements block) translateStatement

translateStatement :: Statement -> Translator ()
translateStatement (StatementBlock block) = translateBlock block
translateStatement (StatementFunctionCall fcall) = do
  retType <- functionCall fcall
  case retType of
    Nothing -> pure ()
    Just _ -> addOp OpPop
translateStatement (StatementWhile e block) = do
  labelLoopBegin <- newLabel
  labelAfterLoop <- newLabel
  addOp $ OpLabel labelLoopBegin
  translateExpression e
  addOp $ OpJumpIfZero labelAfterLoop
  translateBlock block
  addOp $ OpJump labelLoopBegin
  addOp $ OpLabel labelAfterLoop
translateStatement (StatementAssign v expr) = do
  translateExpression expr
  storeVar v
translateStatement (StatementIfElse e blockTrue blockFalse) = do
  labelElseBranch <- newLabel
  labelAfterIf <- newLabel
  translateExpression e
  addOp $ OpJumpIfZero labelElseBranch
  translateBlock blockTrue
  addOp $ OpJump labelAfterIf
  addOp $ OpLabel labelElseBranch
  translateBlock blockFalse
  addOp $ OpLabel labelAfterIf
translateStatement (StatementReturn Nothing) = addOp OpReturn
translateStatement (StatementReturn (Just expr)) = do
  translateExpression expr
  addOp OpReturn

translateArgs :: [Expr] -> Translator ()
translateArgs args =
  forM_ (reverse args) translateExpression

generateNewForeignFunction :: String
                           -> Maybe VarType
                           -> [VarType]
                           -> Translator FunID
generateNewForeignFunction name rettype params = do
  FunID f <- (inc . lastForeignFun) <$> State.get
  State.modify $
    \env ->
       env
       { lastForeignFun = FunID f
       , foreignFuns = IntMap.insert f (ffdecl $ FunID f) $ foreignFuns env
       }
  pure $ FunID f
  where
    inc (FunID f) = FunID (f + 1)
    ffdecl f =
      ForeignFunctionDecl
      { foreignFunDeclRetType = rettype
      , foreignFunDeclName = f
      , foreignFunDeclRealName = name
      , foreignFunDeclParams = params
      }

getPrintfDesc :: [VarType] -> String
getPrintfDesc = concatMap desc
  where
    desc VarTypeInt = "%ld"
    desc VarTypeFloat = "%g"
    desc VarTypeString = "%s"
    desc VarTypePtr = error "Type mismatch"

printCall :: [Expr] -> Translator ()
printCall [] = pure ()
printCall args = do
  translateArgs args
  let types = map exprType args
  cid <- newConstant $ ValueString $ Right $ getPrintfDesc types
  addOp (OpPushString cid)
  f <- generateNewForeignFunction "printf" Nothing (VarTypeString : types)
  addOp $ OpForeignCall f

functionCall :: FunctionCall -> Translator (Maybe VarType)
functionCall (PrintCall args) = printCall args >> pure Nothing
functionCall f@ForeignFunctionCall{} = do
  translateArgs $ foreignFunCallArgs f
  addOp $ OpForeignCall (foreignFunCallName f)
  pure $ foreignFunCallRetType f
functionCall f@NativeFunctionCall{} = do
  translateCapturesForCall $ nativeFunCallCaptures f
  translateArgs $ nativeFunCallArgs f
  addOp $ OpCall $ nativeFunCallName f
  pure $ nativeFunCallRetType f

localTranslate :: Translator a -> Translator (a, BytecodeFunction)
localTranslate m = Trans.lift $ Writer.runWriterT m

newConstant :: Value -> Translator ConstID
newConstant val = do
  newConst@(ConstID cid) <- (inc . lastConst) <$> State.get
  State.modify $
    \env ->
       env
       { lastConst = newConst
       , consts = IntMap.insert cid val (consts env)
       }
  pure newConst
  where
    inc (ConstID cid) = ConstID (cid + 1)

getBinOp :: BinOp -> VarType -> Op
getBinOp BinPlus VarTypeInt = OpPlusInt
getBinOp BinPlus VarTypeFloat = OpPlusFloat
getBinOp BinPlus _ = error "Type mismatch"
getBinOp BinMinus VarTypeInt = OpMinusInt
getBinOp BinMinus VarTypeFloat = OpMinusFloat
getBinOp BinMinus _ = error "Type mismatch"
getBinOp BinTimes VarTypeInt = OpTimesInt
getBinOp BinTimes VarTypeFloat = OpTimesFloat
getBinOp BinTimes _ = error "Type mismatch"
getBinOp BinDiv VarTypeInt = OpDivInt
getBinOp BinDiv VarTypeFloat = OpDivFloat
getBinOp BinDiv _ = error "Type mismatch"
getBinOp BinMod VarTypeInt = OpModInt
getBinOp BinMod _ = error "Type mismatch"
getBinOp BinBitAnd VarTypeInt = OpBitAndInt
getBinOp BinBitAnd _ = error "Type mismatch"
getBinOp BinBitOr VarTypeInt = OpBitOrInt
getBinOp BinBitOr _ = error "Type mismatch"
getBinOp BinBitXor VarTypeInt = OpBitXorInt
getBinOp BinBitXor _ = error "Type mismatch"
getBinOp BinAnd VarTypeInt = OpAndInt
getBinOp BinAnd _ = error "Type mismatch"
getBinOp BinOr VarTypeInt = OpOrInt
getBinOp BinOr _ = error "Type mismatch"
getBinOp BinEq VarTypeInt = OpEqInt
getBinOp BinEq VarTypeFloat = OpEqFloat
getBinOp BinEq _ = error "Type mismatch"
getBinOp BinLt VarTypeInt = OpLtInt
getBinOp BinLt VarTypeFloat = OpLtFloat
getBinOp BinLt _ = error "Type mismatch"

getUnOp :: UnOp -> VarType -> Op
getUnOp UnNeg VarTypeInt = OpNegateInt
getUnOp UnNeg VarTypeFloat = OpNegateFloat
getUnOp UnNeg _ = error "Type mismatch"
getUnOp UnNot VarTypeInt = OpNotInt
getUnOp UnNot _ = error "Type mismatch"
getUnOp UnIntToFloat VarTypeInt = OpIntToFloat
getUnOp UnIntToFloat _ = error "Type mismatch"

translateExpression :: Expr -> Translator ()
translateExpression (ExprFunctionCall fcall) = do
  _ <- functionCall fcall
  pure ()
translateExpression (ExprVar _ vname) = loadVar vname
translateExpression (ExprInt i) = do
  cid <- newConstant $ ValueInt i
  addOp (OpPushInt cid)
translateExpression (ExprFloat f) = do
  cid <- newConstant $ ValueFloat f
  addOp (OpPushFloat cid)
translateExpression (ExprString s) = do
  cid <- newConstant $ ValueString $ Right s
  addOp (OpPushString cid)
translateExpression (ExprUnOp op e) = do
  translateExpression e
  addOp $ getUnOp op $ exprType e
translateExpression e@(ExprBinOp op lhs rhs) = do
  translateExpression rhs
  translateExpression lhs
  addOp $ getBinOp op $ exprType e
translateExpression _ = undefined -- TODO: Remove when pattern synonyms have COMPLETE pragma.

translateCapturesForCall :: [VarID] -> Translator ()
translateCapturesForCall vars = do
  currentCaptures <- captureMap <$> State.get
  forM_ (reverse vars) $ \(VarID vid) -> do
    case IntMap.lookup vid currentCaptures of
      Nothing -> addOp $ OpAddressOf (VarID vid)
      Just v -> addOp $ OpLoad v

translateCapturesInBody :: [VarID] -> Translator ()
translateCapturesInBody vars = do
  vs <- mapM (const $ newVariable VarTypePtr) vars
  forM_ vs (addOp . OpStore)
  State.modify $ \env -> env
    { captureMap = IntMap.fromList $ zip (map (\(VarID vid) -> vid) vars) vs }
