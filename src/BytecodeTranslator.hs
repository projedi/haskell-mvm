module BytecodeTranslator
  ( translate
  ) where

import Control.Monad
import Control.Monad.State (State)
import qualified Control.Monad.State as State
import qualified Control.Monad.Trans as Trans
import Control.Monad.Writer (WriterT)
import qualified Control.Monad.Writer as Writer
import Data.Foldable (asum)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map

import Bytecode
import Syntax
import Value (Value(..))

translate :: Program -> Bytecode
translate (Program block libs) =
  let (bc, finalEnv) =
        runTranslator (translateBlock block) emptyEnv
  in bc
     { bytecodeLibraries = libs
     , bytecodeConstants = consts finalEnv
     }

data Layer = Layer
  { varEnv :: Map VarName VarID
  , funEnv :: Map FunctionName FunID
  }

findVariableInLayer :: Layer -> VarName -> Maybe VarID
findVariableInLayer Layer {varEnv = lvars} vname = Map.lookup vname lvars

newVariableInLayer :: Layer -> VarName -> VarID -> Maybe Layer
newVariableInLayer l@Layer {varEnv = lvars} vname vid =
  case Map.insertLookupWithKey (\_ val _ -> val) vname vid lvars of
    (Nothing, lvars') ->
      Just $
      l
      { varEnv = lvars'
      }
    (Just _, _) -> Nothing

findFunctionInLayer :: Layer -> FunctionName -> Maybe FunID
findFunctionInLayer Layer {funEnv = lfuns} fname = Map.lookup fname lfuns

newFunctionInLayer :: Layer -> FunctionName -> FunID -> Maybe Layer
newFunctionInLayer l@Layer {funEnv = lfuns} fname fid =
  case Map.insertLookupWithKey (\_ val _ -> val) fname fid lfuns of
    (Nothing, lfuns') ->
      Just $
      l
      { funEnv = lfuns'
      }
    (Just _, _) -> Nothing

emptyLayer :: Layer
emptyLayer =
  Layer
  { varEnv = Map.empty
  , funEnv = Map.empty
  }

data Env = Env
  { currentFunction :: FunID
  , currentRetType :: Maybe VarType
  , lastLabel :: LabelID
  , lastVar :: VarID
  , lastFun :: FunID
  , lastConst :: ConstID
  , layers :: [Layer]
  , vars :: IntMap VarType
  , funs :: IntMap (FunctionDecl, Bool) -- True if already defined
  , consts :: IntMap Value
  }

emptyEnv :: Env
emptyEnv =
  Env
  { currentFunction = FunID 0
  , currentRetType = Nothing
  , lastLabel = LabelID (-1)
  , lastVar = VarID (-1)
  , lastFun = FunID 0
  , lastConst = ConstID (-1)
  , layers = [emptyLayer]
  , vars = IntMap.empty
  , funs = IntMap.empty
  , consts = IntMap.empty
  }

findVariableInEnv :: Env -> VarName -> (VarType, VarID)
findVariableInEnv env vname =
  let (Just (VarID v)) = asum (map (`findVariableInLayer` vname) (layers env))
      (Just vtype) = IntMap.lookup v (vars env)
  in (vtype, VarID v)

newVariableInEnv :: Env -> VarType -> (VarID, Env)
newVariableInEnv env vtype =
  let (VarID v) = inc $ lastVar env
      (Nothing, vars') =
        IntMap.insertLookupWithKey (\_ val _ -> val) v vtype (vars env)
  in ( VarID v
     , env
       { lastVar = VarID v
       , vars = vars'
       })
  where
    inc (VarID lastVarID) = VarID (lastVarID + 1)

introduceVariableInEnv :: Env -> VarDecl -> (VarID, Env)
introduceVariableInEnv env (VarDecl vtype vname) =
  let (v, env'@Env {layers = (l:ls)}) = newVariableInEnv env vtype
      Just l' = newVariableInLayer l vname v
  in ( v
     , env'
       { layers = l' : ls
       })

checkExistingFunctionInEnv :: Env -> FunID -> FunctionDecl -> (FunID, Env)
checkExistingFunctionInEnv env (FunID fid) fdecl =
  let Just (existingDecl, _) = IntMap.lookup fid $ funs env
  in if declMatch existingDecl fdecl
       then (FunID fid, env)
       else error "Type mismatch"
  where
    declMatch (FunctionDecl lrettype _ lparams) (FunctionDecl rrettype _ rparams) =
      (lrettype == rrettype) && lparams == rparams

newFunctionInEnv :: Env -> FunctionDecl -> (FunID, Env)
newFunctionInEnv env fdecl =
  let (FunID f) = inc $ lastFun env
      (Nothing, funs') =
        IntMap.insertLookupWithKey (\_ val _ -> val) f (fdecl, False) (funs env)
  in ( FunID f
     , env
       { lastFun = FunID f
       , funs = funs'
       })
  where
    inc (FunID lastFunID) = FunID (lastFunID + 1)

introduceFunctionInEnv :: Env -> FunctionDecl -> (FunID, Env)
introduceFunctionInEnv env fdecl@(FunctionDecl _ fname _) =
  case findFunctionInLayer (head $ layers env) fname of
    Just fid -> checkExistingFunctionInEnv env fid fdecl
    Nothing ->
      let (f, env'@Env {layers = l:ls}) = newFunctionInEnv env fdecl
          Just l' = newFunctionInLayer l fname f
      in ( f
         , env'
           { layers = l' : ls
           })

markFunctionAsDefinedInEnv :: Env -> FunID -> Env
markFunctionAsDefinedInEnv env (FunID fid) =
  env
  { funs = go $ funs env
  }
  where
    go = IntMap.alter (\(Just (fdecl, False)) -> Just (fdecl, True)) fid

findFunctionInEnv :: Env -> FunctionName -> (FunID, FunctionDecl)
findFunctionInEnv env fname =
  let (Just (FunID fid)) = asum (map (`findFunctionInLayer` fname) (layers env))
      (Just (f, _)) = IntMap.lookup fid (funs env)
  in (FunID fid, f)

-- TODO: Consider reusing FunID, LabelID, VarID.
startBlockInEnv :: Env -> Env
startBlockInEnv env =
  env
  { layers = emptyLayer : layers env
  }

stopBlockInEnv :: Env -> Env -> Env
stopBlockInEnv _ env@Env {layers = _:ls} =
  env
  { layers = ls
  }
stopBlockInEnv _ _ = error "Env broke"

type Translator a = WriterT Bytecode (State Env) a

runTranslator :: Translator a -> Env -> (Bytecode, Env)
runTranslator m = State.runState (Writer.execWriterT m)

addCodeTo :: BytecodeFunction -> FunID -> Translator ()
addCodeTo code (FunID fid) =
  Writer.tell $ Bytecode (IntMap.singleton fid code) [] IntMap.empty

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

findVariable :: VarName -> Translator (VarType, VarID)
findVariable v = do
  env <- State.get
  pure $ findVariableInEnv env v

newVariable :: VarType -> Translator VarID
newVariable vtype = do
  env <- State.get
  let (v, env') = newVariableInEnv env vtype
  State.put env'
  addOp $ OpIntroVar v vtype
  pure v

introduceVariable :: VarDecl -> Translator VarID
introduceVariable vdecl@(VarDecl vtype _) = do
  env <- State.get
  let (v, env') = introduceVariableInEnv env vdecl
  State.put env'
  addOp $ OpIntroVar v vtype
  pure v

introduceFunction :: FunctionDecl -> Translator FunID
introduceFunction fdecl = do
  env <- State.get
  let (f, env') = introduceFunctionInEnv env fdecl
  State.put env'
  pure f

markFunctionAsDefined :: FunID -> Translator ()
markFunctionAsDefined f = State.modify (`markFunctionAsDefinedInEnv` f)

namespaceBlock :: Translator a -> Translator a
namespaceBlock m = do
  envBefore <- State.get
  State.put $ startBlockInEnv envBefore
  res <- m
  envAfter <- State.get
  State.put $ stopBlockInEnv envBefore envAfter
  pure res

translateFunctionBody :: FunctionDecl -> FunID -> Translator () -> Translator ()
translateFunctionBody (FunctionDecl retType _ _) fid body = do
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

translateNativeFunctionBody :: FunctionDecl
                            -> FunID
                            -> [VarName]
                            -> Block
                            -> Translator ()
translateNativeFunctionBody fdecl@(FunctionDecl _ _ params) fid paramNames body =
  translateFunctionBody fdecl fid $
  namespaceBlock $
  do vs <- mapM introduceVariable $ paramDecls params paramNames
     forM_ vs (addOp . OpStore)
     translateStatement $ StatementBlock body
  where
    paramDecls [] [] = []
    paramDecls (t:ts) (n:ns) = VarDecl t n : paramDecls ts ns
    paramDecls _ _ = error "Type mismatch"

translateForeignFunctionBody :: FunctionDecl -> FunID -> Translator ()
translateForeignFunctionBody fdecl@(FunctionDecl retType (FunctionName fname) params) fid =
  translateFunctionBody fdecl fid $
  do embedExpressionTranslator $
       addOpWithoutType $ OpForeignCall fname retType params

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
translateStatement (StatementAssign vname expr) = do
  eType <- embedExpression expr
  (vType, v) <- findVariable vname
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
translateStatement (StatementFor vname eFrom eTo block) =
  namespaceBlock $
  do (vType, v) <- findVariable vname
     when (vType /= VarTypeInt) $ error "Type mismatch"
     vCur <- newVariable VarTypeInt
     eFromType <- embedExpression eFrom
     when (eFromType /= VarTypeInt) $ error "Type mismatch"
     addOp $ OpStore vCur
     vTo <- newVariable VarTypeInt
     eToType <- embedExpression eTo
     when (eToType /= VarTypeInt) $ error "Type mismatch"
     addOp $ OpStore vTo
     labelLoopBegin <- newLabel
     labelAfterLoop <- newLabel
     addOp $ OpLabel labelLoopBegin
     addOp $ OpLoad vCur
     addOp $ OpLoad vTo
     addOp OpLtInt
     addOp OpNotInt
     -- if vTo < v then loop is finished.
     addOp $ OpJumpIfZero labelAfterLoop
     addOp $ OpLoad vCur
     addOp $ OpStore v
     translateBlock block
     addOp $ OpLoad vCur
     cid <- embedExpressionTranslator $ newConstant (ValueInt 1)
     addOp $ OpPushInt cid
     addOp OpPlusInt
     addOp $ OpStore vCur
     addOp $ OpJump labelLoopBegin
     addOp $ OpLabel labelAfterLoop
translateStatement (StatementFunctionDecl f) = introduceFunction f >> pure ()
translateStatement (StatementFunctionDef f paramNames body) = do
  fid <- introduceFunction f
  markFunctionAsDefined fid
  translateNativeFunctionBody f fid paramNames body
translateStatement (StatementForeignFunctionDecl f) = do
  fid <- introduceFunction f
  markFunctionAsDefined fid
  translateForeignFunctionBody f fid
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

findFunction :: FunctionName -> ExpressionTranslator (FunID, FunctionDecl)
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

generatePrintfDesc :: [VarType] -> ExpressionTranslator ()
generatePrintfDesc types = do
  cid <- newConstant $ ValueString $ Right str
  addOpWithoutType (OpPushString cid)
  where
    desc VarTypeInt = "%ld"
    desc VarTypeFloat = "%g"
    desc VarTypeString = "%s"
    str = concatMap desc types

printCall :: [Expr] -> ExpressionTranslator ()
printCall [] = pure ()
printCall args = do
  valsWithTypes <- mapM (localTranslate . translateExpression) args
  forM_ (reverse (map snd valsWithTypes)) addCode
  let types = map fst valsWithTypes
  generatePrintfDesc types
  addOpWithoutType $ OpForeignCall "printf" Nothing (VarTypeString : types)

functionCall :: FunctionCall -> ExpressionTranslator (Maybe VarType)
functionCall (FunctionCall (FunctionName "print") args) =
  printCall args >> pure Nothing
functionCall (FunctionCall fname args) = do
  (fid, FunctionDecl rettype _ params) <- findFunction fname
  translateArgs args params
  addOpWithoutType $ OpCall fid
  pure rettype

loadVar :: VarName -> ExpressionTranslator VarType
loadVar vname = do
  env <- State.get
  let (vtype, vid) = findVariableInEnv env vname
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
opRetType (OpForeignCall _ _ _) = error "Type mismatch"
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
