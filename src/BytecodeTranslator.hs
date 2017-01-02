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
import Data.Map (Map)
import qualified Data.Map as Map

import Bytecode
import Syntax
import Value (Value(..))

translate :: Program -> Bytecode
translate p =
  bc
     { bytecodeLibraries = programLibraries p
     , bytecodeConstants = consts finalEnv
     , bytecodeForeignFunctions = foreignFuns finalEnv
     }
  where
    lastForeignFunID fs
      | IntMap.null fs = FunID 0
      | otherwise = FunID $ fst $ IntMap.findMax fs
    initEnv = emptyEnv
      { lastForeignFun = lastForeignFunID $ programForeignFunctions p
      , foreignFuns = programForeignFunctions p
      }
    (bc, finalEnv) = runTranslator (translateBlock (programStatements p)) initEnv

data Env = Env
  { currentFunction :: FunID
  , currentRetType :: Maybe VarType
  , lastLabel :: LabelID
  , lastConst :: ConstID
  , vars :: IntMap VarType
  , funs :: IntMap FunctionDef
  , consts :: IntMap Value
  , foreignFuns :: IntMap ForeignFunctionDecl
  , lastForeignFun :: FunID
  }

emptyEnv :: Env
emptyEnv =
  Env
  { currentFunction = FunID 0
  , currentRetType = Nothing
  , lastLabel = LabelID (-1)
  , lastConst = ConstID (-1)
  , vars = IntMap.empty
  , funs = IntMap.empty
  , consts = IntMap.empty
  , foreignFuns = IntMap.empty
  , lastForeignFun = FunID 0
  }

findVariableInEnv :: Env -> VarID -> VarType
findVariableInEnv env (VarID v) =
  let Just vtype = IntMap.lookup v (vars env)
  in vtype

introduceVariableInEnv :: Env -> VarDecl -> Env
introduceVariableInEnv env (VarDecl vtype (VarID v)) =
  let (Nothing, vars') = IntMap.insertLookupWithKey (\_ val _ -> val) v vtype (vars env)
  in env { vars = vars' }

introduceFunctionInEnv :: Env -> FunctionDef -> Env
introduceFunctionInEnv env fdef =
  let (FunID fid) = funDefName fdef
      funs' = IntMap.insert fid fdef (funs env)
  in env { funs = funs' }

findFunctionInEnv :: Env -> FunID -> FunctionDef
findFunctionInEnv env (FunID fid) =
  let (Just f) = IntMap.lookup fid (funs env)
  in f

type Translator a = WriterT Bytecode (State Env) a

runTranslator :: Translator a -> Env -> (Bytecode, Env)
runTranslator m = State.runState (Writer.execWriterT m)

addCodeTo :: BytecodeFunction -> FunID -> Translator ()
addCodeTo code (FunID fid) =
  Writer.tell $ Bytecode
    { bytecodeFunctions = IntMap.singleton fid code
    , bytecodeLibraries = []
    , bytecodeConstants = IntMap.empty
    , bytecodeForeignFunctions = IntMap.empty
    }

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

addCodeToCurrent :: BytecodeFunction -> Translator ()
addCodeToCurrent code = do
  fid <- currentFunction <$> State.get
  addCodeTo code fid

addOp :: Op -> Translator ()
addOp op = addCodeToCurrent (BytecodeFunction [op])

findVariable :: VarID -> Translator VarType
findVariable v = do
  env <- State.get
  pure $ findVariableInEnv env v

introduceVariable :: VarDecl -> Translator VarID
introduceVariable vdecl@(VarDecl vtype v) = do
  env <- State.get
  let env' = introduceVariableInEnv env vdecl
  State.put env'
  addOp $ OpIntroVar v vtype
  pure v

introduceFunction :: FunctionDef -> Translator ()
introduceFunction fdef = do
  env <- State.get
  let env' = introduceFunctionInEnv env fdef
  State.put env'

namespaceBlock :: Translator a -> Translator a
namespaceBlock m = m

translateFunctionBody :: Maybe VarType -> FunID -> Translator () -> Translator ()
translateFunctionBody retType fid body = do
  envBefore <- State.get
  State.put $
    envBefore
    { currentFunction = fid
    , currentRetType = retType
    }
  body
  envAfter <- State.get
  State.put $
    envAfter
    { currentFunction = currentFunction envBefore
    , currentRetType = currentRetType envBefore
    }

translateNativeFunctionBody :: FunctionDef
                            -> Translator ()
translateNativeFunctionBody f =
  translateFunctionBody (funDefRetType f) (funDefName f) $
  namespaceBlock $
  do vs <- mapM introduceVariable $ funDefParams f
     forM_ vs (addOp . OpStore)
     translateStatement $ StatementBlock $ funDefBody f

translateReturn :: Translator ()
translateReturn = do
  retType <- currentRetType <$> State.get
  case retType of
    Nothing -> addOp OpReturn
    Just _ -> error "Type mismatch"

translateReturnWithValue :: Expr -> Translator ()
translateReturnWithValue expr = do
  retType <- currentRetType <$> State.get
  case retType of
    Nothing -> error "Type mismatch"
    Just expectedType -> do
      actualType <- embedExpression expr
      _ <- embedExpressionTranslator $ convert actualType expectedType
      addOp OpReturn

translateBlock :: Block -> Translator ()
translateBlock block = namespaceBlock $ do
  forM_ (blockVariables block) introduceVariable
  forM_ (blockFunctions block) introduceFunction
  forM_ (blockFunctions block) translateNativeFunctionBody
  forM_ (blockStatements block) translateStatement

translateStatement :: Statement -> Translator ()
translateStatement StatementNoop = pure ()
translateStatement (StatementBlock block) = translateBlock block
translateStatement (StatementFunctionCall fcall) = do
  retType <- embedExpressionTranslator (functionCall fcall)
  case retType of
    Nothing -> pure ()
    Just _ -> addOp OpPop
translateStatement (StatementWhile e block) = do
  labelLoopBegin <- newLabel
  labelAfterLoop <- newLabel
  addOp $ OpLabel labelLoopBegin
  eType <- embedExpression e
  when (eType /= VarTypeInt) $ error "Type mismatch"
  addOp $ OpJumpIfZero labelAfterLoop
  translateBlock block
  addOp $ OpJump labelLoopBegin
  addOp $ OpLabel labelAfterLoop
translateStatement (StatementAssign v expr) = do
  eType <- embedExpression expr
  vType <- findVariable v
  _ <- embedExpressionTranslator (convert eType vType)
  addOp $ OpStore v
translateStatement (StatementIfElse e blockTrue blockFalse) = do
  labelElseBranch <- newLabel
  labelAfterIf <- newLabel
  eType <- embedExpression e
  when (eType /= VarTypeInt) $ error "Type mismatch"
  addOp $ OpJumpIfZero labelElseBranch
  translateBlock blockTrue
  addOp $ OpJump labelAfterIf
  addOp $ OpLabel labelElseBranch
  translateBlock blockFalse
  addOp $ OpLabel labelAfterIf
translateStatement (StatementReturn Nothing) = translateReturn
translateStatement (StatementReturn (Just expr)) = translateReturnWithValue expr

embedExpressionTranslator :: ExpressionTranslator a -> Translator a
embedExpressionTranslator m = do
  env <- State.get
  let ((val, code), env') = State.runState (runExpressionTranslator m) env
  State.put env'
  addCodeToCurrent code
  pure val

embedExpression :: Expr -> Translator VarType
embedExpression e = embedExpressionTranslator (translateExpression e)

type ExpressionTranslator a = WriterT BytecodeFunction (State Env) a

runExpressionTranslator :: ExpressionTranslator a
                        -> State Env (a, BytecodeFunction)
runExpressionTranslator = Writer.runWriterT

findFunction :: FunID -> ExpressionTranslator FunctionDef
findFunction fname = do
  env <- State.get
  pure $ findFunctionInEnv env fname

convertArg :: VarType -> BytecodeFunction -> VarType -> BytecodeFunction
convertArg argType (BytecodeFunction code) paramType =
  case convertOp argType paramType of
    Nothing -> BytecodeFunction code
    Just op -> BytecodeFunction $ code ++ [op]

convertArgs :: [(VarType, BytecodeFunction)] -> [VarType] -> [BytecodeFunction]
convertArgs [] [] = []
convertArgs ((argType, code):args) (paramType:params) =
  convertArg argType code paramType : convertArgs args params
convertArgs _ _ = error "Type mismatch"

translateArgs :: [Expr] -> [VarType] -> ExpressionTranslator ()
translateArgs args types = do
  valsWithTypes <- mapM (localTranslate . translateExpression) args
  let vals = convertArgs valsWithTypes types
  forM_ (reverse vals) addCode

generateNewForeignFunction :: String -> Maybe VarType -> [VarType] -> ExpressionTranslator FunID
generateNewForeignFunction name rettype params = do
  FunID f <- (inc . lastForeignFun) <$> State.get
  State.modify $ \env -> env
    { lastForeignFun = FunID f
    , foreignFuns = IntMap.insert f (ffdecl $ FunID f) $ foreignFuns env
    }
  pure $ FunID f
  where
    inc (FunID f) = FunID (f + 1)
    ffdecl f = ForeignFunctionDecl
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

printCall :: [Expr] -> ExpressionTranslator ()
printCall [] = pure ()
printCall args = do
  valsWithTypes <- mapM (localTranslate . translateExpression) args
  forM_ (reverse (map snd valsWithTypes)) addCode
  let types = map fst valsWithTypes
  cid <- newConstant $ ValueString $ Right $ getPrintfDesc types
  addOpWithoutType (OpPushString cid)
  f <- generateNewForeignFunction "printf" Nothing (VarTypeString : types)
  addOpWithoutType $ OpForeignCall f

functionCall :: FunctionCall -> ExpressionTranslator (Maybe VarType)
functionCall (PrintCall args) =
  printCall args >> pure Nothing
functionCall (ForeignFunctionCall (FunID fid) args) = do
  Just f <- (IntMap.lookup fid . foreignFuns) <$> State.get
  translateArgs args $ foreignFunDeclParams f
  addOpWithoutType $ OpForeignCall (foreignFunDeclName f)
  pure $ foreignFunDeclRetType f
functionCall (NativeFunctionCall fid args) = do
  FunctionDef {funDefRetType = rettype, funDefParams = params} <- findFunction fid
  translateArgs args $ map (\(VarDecl t _) -> t) params
  addOpWithoutType $ OpCall fid
  pure rettype

loadVar :: VarID -> ExpressionTranslator VarType
loadVar vid = do
  env <- State.get
  let vtype = findVariableInEnv env vid
  addOpWithoutType $ OpLoad vid
  pure vtype

localTranslate :: ExpressionTranslator a
               -> ExpressionTranslator (a, BytecodeFunction)
localTranslate m = Trans.lift $ runExpressionTranslator m

convertOp :: VarType -> VarType -> Maybe Op
convertOp VarTypeInt VarTypeInt = Nothing
convertOp VarTypeFloat VarTypeFloat = Nothing
convertOp VarTypeString VarTypeString = Nothing
convertOp VarTypeInt VarTypeFloat = Just OpIntToFloat
convertOp _ _ = error "Type mismatch"

convert :: VarType -> VarType -> ExpressionTranslator VarType
convert tFrom tTo =
  case convertOp tFrom tTo of
    Nothing -> pure tTo
    Just op -> addOpWithType op

addCode :: BytecodeFunction -> ExpressionTranslator ()
addCode = Writer.tell

opRetType :: Op -> VarType
opRetType OpNegateInt = VarTypeInt
opRetType OpPlusInt = VarTypeInt
opRetType OpMinusInt = VarTypeInt
opRetType OpTimesInt = VarTypeInt
opRetType OpDivInt = VarTypeInt
opRetType OpModInt = VarTypeInt
opRetType OpBitAndInt = VarTypeInt
opRetType OpBitOrInt = VarTypeInt
opRetType OpBitXorInt = VarTypeInt
opRetType OpNotInt = VarTypeInt
opRetType OpAndInt = VarTypeInt
opRetType OpOrInt = VarTypeInt
opRetType OpEqInt = VarTypeInt
opRetType OpLtInt = VarTypeInt
opRetType OpNegateFloat = VarTypeFloat
opRetType OpPlusFloat = VarTypeFloat
opRetType OpMinusFloat = VarTypeFloat
opRetType OpTimesFloat = VarTypeFloat
opRetType OpDivFloat = VarTypeFloat
opRetType OpEqFloat = VarTypeInt
opRetType OpLtFloat = VarTypeInt
opRetType OpIntToFloat = VarTypeFloat
opRetType (OpPushInt _) = VarTypeInt
opRetType (OpPushFloat _) = VarTypeFloat
opRetType (OpPushString _) = VarTypeString
opRetType OpPop = error "Type mismatch"
opRetType OpReturn = error "Type mismatch"
opRetType (OpCall _) = error "Type mismatch"
opRetType (OpForeignCall _) = error "Type mismatch"
opRetType (OpLabel _) = error "Type mismatch"
opRetType (OpJump _) = error "Type mismatch"
opRetType (OpJumpIfZero _) = error "Type mismatch"
opRetType (OpStore _) = error "Type mismatch"
opRetType (OpLoad _) = error "Type mismatch"
opRetType (OpIntroVar _ _) = error "Type mismatch"

addOpWithoutType :: Op -> ExpressionTranslator ()
addOpWithoutType op = addCode (BytecodeFunction [op])

addOpWithType :: Op -> ExpressionTranslator VarType
addOpWithType op = addOpWithoutType op >> pure (opRetType op)

newConstant :: Value -> ExpressionTranslator ConstID
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

translateBinOp :: Expr
               -> Expr
               -> Map (VarType, VarType) (VarType, Op)
               -> ExpressionTranslator VarType
translateBinOp lhs rhs table = do
  rhsType <- translateExpression rhs
  (lhsType, code) <- localTranslate $ translateExpression lhs
  case Map.lookup (lhsType, rhsType) table of
    Nothing -> error "Type mismatch"
    Just (resType, op) -> do
      _ <- convert rhsType resType
      addCode code
      _ <- convert lhsType resType
      addOpWithType op

translateExpression :: Expr -> ExpressionTranslator VarType
translateExpression (ExprFunctionCall fcall) = do
  Just rettype <- functionCall fcall
  pure rettype
translateExpression (ExprVar vname) = loadVar vname
translateExpression (ExprInt i) = do
  cid <- newConstant $ ValueInt i
  addOpWithType (OpPushInt cid)
translateExpression (ExprFloat f) = do
  cid <- newConstant $ ValueFloat f
  addOpWithType (OpPushFloat cid)
translateExpression (ExprString s) = do
  cid <- newConstant $ ValueString $ Right s
  addOpWithType (OpPushString cid)
translateExpression (ExprNeg e) = do
  eType <- translateExpression e
  case eType of
    VarTypeInt -> addOpWithType OpNegateInt
    VarTypeFloat -> addOpWithType OpNegateFloat
    _ -> error "Type mismatch"
translateExpression (ExprNot e) = do
  eType <- translateExpression e
  case eType of
    VarTypeInt -> addOpWithType OpNotInt
    _ -> error "Type mismatch"
translateExpression (ExprPlus lhs rhs) =
  translateBinOp lhs rhs $
  Map.fromList
    [ ((VarTypeInt, VarTypeInt), (VarTypeInt, OpPlusInt))
    , ((VarTypeInt, VarTypeFloat), (VarTypeFloat, OpPlusFloat))
    , ((VarTypeFloat, VarTypeInt), (VarTypeFloat, OpPlusFloat))
    , ((VarTypeFloat, VarTypeFloat), (VarTypeFloat, OpPlusFloat))
    ]
translateExpression (ExprMinus lhs rhs) =
  translateBinOp lhs rhs $
  Map.fromList
    [ ((VarTypeInt, VarTypeInt), (VarTypeInt, OpMinusInt))
    , ((VarTypeInt, VarTypeFloat), (VarTypeFloat, OpMinusFloat))
    , ((VarTypeFloat, VarTypeInt), (VarTypeFloat, OpMinusFloat))
    , ((VarTypeFloat, VarTypeFloat), (VarTypeFloat, OpMinusFloat))
    ]
translateExpression (ExprTimes lhs rhs) =
  translateBinOp lhs rhs $
  Map.fromList
    [ ((VarTypeInt, VarTypeInt), (VarTypeInt, OpTimesInt))
    , ((VarTypeInt, VarTypeFloat), (VarTypeFloat, OpTimesFloat))
    , ((VarTypeFloat, VarTypeInt), (VarTypeFloat, OpTimesFloat))
    , ((VarTypeFloat, VarTypeFloat), (VarTypeFloat, OpTimesFloat))
    ]
translateExpression (ExprDiv lhs rhs) =
  translateBinOp lhs rhs $
  Map.fromList
    [ ((VarTypeInt, VarTypeInt), (VarTypeInt, OpDivInt))
    , ((VarTypeInt, VarTypeFloat), (VarTypeFloat, OpDivFloat))
    , ((VarTypeFloat, VarTypeInt), (VarTypeFloat, OpDivFloat))
    , ((VarTypeFloat, VarTypeFloat), (VarTypeFloat, OpDivFloat))
    ]
translateExpression (ExprMod lhs rhs) =
  translateBinOp lhs rhs $
  Map.fromList [((VarTypeInt, VarTypeInt), (VarTypeInt, OpModInt))]
translateExpression (ExprBitAnd lhs rhs) =
  translateBinOp lhs rhs $
  Map.fromList [((VarTypeInt, VarTypeInt), (VarTypeInt, OpBitAndInt))]
translateExpression (ExprBitOr lhs rhs) =
  translateBinOp lhs rhs $
  Map.fromList [((VarTypeInt, VarTypeInt), (VarTypeInt, OpBitOrInt))]
translateExpression (ExprBitXor lhs rhs) =
  translateBinOp lhs rhs $
  Map.fromList [((VarTypeInt, VarTypeInt), (VarTypeInt, OpBitXorInt))]
translateExpression (ExprAnd lhs rhs) =
  translateBinOp lhs rhs $
  Map.fromList [((VarTypeInt, VarTypeInt), (VarTypeInt, OpAndInt))]
translateExpression (ExprOr lhs rhs) =
  translateBinOp lhs rhs $
  Map.fromList [((VarTypeInt, VarTypeInt), (VarTypeInt, OpOrInt))]
translateExpression (ExprEq lhs rhs) =
  translateBinOp lhs rhs $
  Map.fromList
    [ ((VarTypeInt, VarTypeInt), (VarTypeInt, OpEqInt))
    , ((VarTypeInt, VarTypeFloat), (VarTypeFloat, OpEqFloat))
    , ((VarTypeFloat, VarTypeInt), (VarTypeFloat, OpEqFloat))
    , ((VarTypeFloat, VarTypeFloat), (VarTypeFloat, OpEqFloat))
    ]
translateExpression (ExprLt lhs rhs) =
  translateBinOp lhs rhs $
  Map.fromList
    [ ((VarTypeInt, VarTypeInt), (VarTypeInt, OpLtInt))
    , ((VarTypeInt, VarTypeFloat), (VarTypeFloat, OpLtFloat))
    , ((VarTypeFloat, VarTypeInt), (VarTypeFloat, OpLtFloat))
    , ((VarTypeFloat, VarTypeFloat), (VarTypeFloat, OpLtFloat))
    ]
