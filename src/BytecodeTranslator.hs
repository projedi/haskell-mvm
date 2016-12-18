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

translate :: Program -> Bytecode
translate (Program stmts) =
  execTranslator (translateStatement (StatementBlock stmts))

data Function
  = FunctionNative FunctionDecl
                   FunID
  | FunctionForeign FunctionDecl

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
  , layers :: [Layer]
  , vars :: IntMap VarType
  , funs :: IntMap (Either FunctionDecl Function)
  }

emptyEnv :: Env
emptyEnv =
  Env
  { currentFunction = FunID 0
  , currentRetType = Nothing
  , lastLabel = LabelID (-1)
  , lastVar = VarID (-1)
  , lastFun = FunID 0
  , layers = [emptyLayer]
  , vars = IntMap.empty
  , funs = IntMap.empty
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
  let Just f = IntMap.lookup fid $ funs env
      existingDecl = getDecl f
  in if declMatch existingDecl fdecl
       then (FunID fid, env)
       else error "Type mismatch"
  where
    getDecl (Left decl) = decl
    getDecl (Right (FunctionNative decl _)) = decl
    getDecl (Right (FunctionForeign decl)) = decl
    declMatch (FunctionDecl lrettype _ lparams) (FunctionDecl rrettype _ rparams) =
      (lrettype == rrettype) && paramsMatch lparams rparams
    paramsMatch [] [] = True
    paramsMatch (VarDecl l _:ls) (VarDecl r _:rs) = l == r && paramsMatch ls rs
    paramsMatch _ _ = False

newFunctionInEnv :: Env -> FunctionDecl -> (FunID, Env)
newFunctionInEnv env fdecl =
  let (FunID f) = inc $ lastFun env
      (Nothing, funs') =
        IntMap.insertLookupWithKey (\_ val _ -> val) f (Left fdecl) (funs env)
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

markFunctionAsForeignInEnv :: Env -> FunID -> Env
markFunctionAsForeignInEnv env (FunID fid) =
  env
  { funs = go $ funs env
  }
  where
    go =
      IntMap.alter
        (\(Just (Left fdecl)) -> Just $ Right $ FunctionForeign fdecl)
        fid

markFunctionAsDefinedInEnv :: Env -> FunID -> Env
markFunctionAsDefinedInEnv env (FunID fid) =
  env
  { funs = go $ funs env
  }
  where
    go =
      IntMap.alter
        (\(Just (Left fdecl)) -> Just $ Right $ FunctionNative fdecl (FunID fid))
        fid

findFunctionInEnv :: Env -> FunctionName -> Function
findFunctionInEnv env fname =
  let (Just (FunID fid)) = asum (map (`findFunctionInLayer` fname) (layers env))
      (Just (Right f)) = IntMap.lookup fid (funs env)
  in f

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

execTranslator :: Translator a -> Bytecode
execTranslator m = State.evalState (Writer.execWriterT m) emptyEnv

addCodeTo :: BytecodeFunction -> FunID -> Translator ()
addCodeTo code (FunID fid) = Writer.tell $ Bytecode (IntMap.singleton fid code)

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

markFunctionAsForeign :: FunID -> Translator ()
markFunctionAsForeign f = State.modify (`markFunctionAsForeignInEnv` f)

namespaceBlock :: Translator a -> Translator a
namespaceBlock m = do
  envBefore <- State.get
  State.put $ startBlockInEnv envBefore
  res <- m
  envAfter <- State.get
  State.put $ stopBlockInEnv envBefore envAfter
  pure res

translateFunctionBody :: FunctionDecl -> FunID -> [Statement] -> Translator ()
translateFunctionBody (FunctionDecl retType _ params) fid body = do
  envBefore <- State.get
  State.put $
    envBefore
    { currentFunction = fid
    , currentRetType = retType
    }
  namespaceBlock $
    do vs <- mapM introduceVariable params
       forM_ vs (addOp . OpStore)
       translateStatement $ StatementBlock body
  envAfter <- State.get
  State.put $
    envAfter
    { currentFunction = currentFunction envBefore
    , currentRetType = currentRetType envBefore
    }

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

translateStatement :: Statement -> Translator ()
translateStatement (StatementBlock stmts) = namespaceBlock $ forM_ stmts translateStatement
translateStatement (StatementFunctionCall fcall) = do
  retType <- embedExpressionTranslator (functionCall fcall)
  case retType of
    Nothing -> pure ()
    Just _ -> addOp OpPop
translateStatement (StatementWhile e stmt) = do
  labelLoopBegin <- newLabel
  labelAfterLoop <- newLabel
  addOp $ OpLabel labelLoopBegin
  eType <- embedExpression e
  when (eType /= VarTypeInt) $ error "Type mismatch"
  addOp $ OpJumpIfZero labelAfterLoop
  translateStatement stmt
  addOp $ OpJump labelLoopBegin
  addOp $ OpLabel labelAfterLoop
translateStatement (StatementAssign vname expr) = do
  eType <- embedExpression expr
  (vType, v) <- findVariable vname
  _ <- embedExpressionTranslator (convert eType vType)
  addOp $ OpStore v
translateStatement (StatementIfElse e stmtTrue stmtFalse) = do
  labelElseBranch <- newLabel
  labelAfterIf <- newLabel
  eType <- embedExpression e
  when (eType /= VarTypeInt) $ error "Type mismatch"
  addOp $ OpJumpIfZero labelElseBranch
  translateStatement stmtTrue
  addOp $ OpJump labelAfterIf
  addOp $ OpLabel labelElseBranch
  translateStatement stmtFalse
  addOp $ OpLabel labelAfterIf
translateStatement (StatementIf e stmtTrue) = do
  labelAfterIf <- newLabel
  eType <- embedExpression e
  when (eType /= VarTypeInt) $ error "Type mismatch"
  addOp $ OpJumpIfZero labelAfterIf
  translateStatement stmtTrue
  addOp $ OpLabel labelAfterIf
translateStatement (StatementFor vname eFrom eTo stmt) =
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
     translateStatement stmt
     addOp $ OpLoad vCur
     addOp $ OpPushInt 1
     addOp OpPlusInt
     addOp $ OpStore vCur
     addOp $ OpJump labelLoopBegin
     addOp $ OpLabel labelAfterLoop
translateStatement (StatementVarDecl v) = introduceVariable v >> pure ()
translateStatement (StatementFunctionDecl f) = introduceFunction f >> pure ()
translateStatement (StatementFunctionDef f body) = do
  fid <- introduceFunction f
  markFunctionAsDefined fid
  translateFunctionBody f fid body
translateStatement (StatementForeignFunctionDecl f) = do
  fid <- introduceFunction f
  markFunctionAsForeign fid
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

findFunction :: FunctionName -> ExpressionTranslator Function
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

builtinCall :: Op -> [Expr] -> ExpressionTranslator ()
builtinCall op args = do
  let count = length args
  translateArgs args $ replicate count VarTypeString
  addOpWithoutType $ OpPushInt count
  addOpWithoutType op

printCall :: [Expr] -> ExpressionTranslator ()
printCall = builtinCall OpPrintCall

dlopenCall :: [Expr] -> ExpressionTranslator ()
dlopenCall = builtinCall OpDlopenCall

foreignFunctionCall :: FunctionDecl -> [Expr] -> ExpressionTranslator ()
foreignFunctionCall (FunctionDecl _ (FunctionName fname) params) args = do
  translateArgs args $ map (\(VarDecl vtype _) -> vtype) params
  addOpWithoutType $ OpForeignCall fname

nativeFunctionCall :: FunctionDecl -> FunID -> [Expr] -> ExpressionTranslator ()
nativeFunctionCall (FunctionDecl _ _ params) fid args = do
  translateArgs args $ map (\(VarDecl vtype _) -> vtype) params
  addOpWithoutType $ OpCall fid

functionCall :: FunctionCall -> ExpressionTranslator (Maybe VarType)
functionCall (FunctionCall (FunctionName "print") args) =
  printCall args >> pure Nothing
functionCall (FunctionCall (FunctionName "dlopen") args) =
  dlopenCall args >> pure Nothing
functionCall (FunctionCall fname args) = do
  f <- findFunction fname
  case f of
    FunctionForeign fdecl@(FunctionDecl rettype _ _) ->
      foreignFunctionCall fdecl args >> pure rettype
    FunctionNative fdecl@(FunctionDecl rettype _ _) fid ->
      nativeFunctionCall fdecl fid args >> pure rettype

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
convertOp VarTypeInt VarTypeString = Just OpIntToString
convertOp VarTypeFloat VarTypeString = Just OpFloatToString
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
opRetType OpNotInt = VarTypeInt
opRetType OpEqInt = VarTypeInt
opRetType OpLtInt = VarTypeInt
opRetType OpNegateFloat = VarTypeFloat
opRetType OpPlusFloat = VarTypeFloat
opRetType OpMinusFloat = VarTypeFloat
opRetType OpTimesFloat = VarTypeFloat
opRetType OpDivFloat = VarTypeFloat
opRetType OpEqFloat = VarTypeInt
opRetType OpLtFloat = VarTypeInt
opRetType OpPlusString = VarTypeString
opRetType OpEqString = VarTypeInt
opRetType OpLtString = VarTypeInt
opRetType OpIntToFloat = VarTypeFloat
opRetType OpIntToString = VarTypeString
opRetType OpFloatToString = VarTypeString
opRetType (OpPushInt _) = VarTypeInt
opRetType (OpPushFloat _) = VarTypeFloat
opRetType (OpPushString _) = VarTypeString
opRetType OpPop = error "Type mismatch"
opRetType OpReturn = error "Type mismatch"
opRetType OpPrintCall = error "Type mismatch"
opRetType OpDlopenCall = error "Type mismatch"
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
translateExpression (ExprInt i) = addOpWithType (OpPushInt i)
translateExpression (ExprFloat f) = addOpWithType (OpPushFloat f)
translateExpression (ExprString s) = addOpWithType (OpPushString s)
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
    , ((VarTypeString, VarTypeString), (VarTypeString, OpPlusString))
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
translateExpression (ExprEq lhs rhs) =
  translateBinOp lhs rhs $
  Map.fromList
    [ ((VarTypeInt, VarTypeInt), (VarTypeInt, OpEqInt))
    , ((VarTypeInt, VarTypeFloat), (VarTypeFloat, OpEqFloat))
    , ((VarTypeFloat, VarTypeInt), (VarTypeFloat, OpEqFloat))
    , ((VarTypeFloat, VarTypeFloat), (VarTypeFloat, OpEqFloat))
    , ((VarTypeString, VarTypeString), (VarTypeString, OpEqString))
    ]
translateExpression (ExprLt lhs rhs) =
  translateBinOp lhs rhs $
  Map.fromList
    [ ((VarTypeInt, VarTypeInt), (VarTypeInt, OpLtInt))
    , ((VarTypeInt, VarTypeFloat), (VarTypeFloat, OpLtFloat))
    , ((VarTypeFloat, VarTypeInt), (VarTypeFloat, OpLtFloat))
    , ((VarTypeFloat, VarTypeFloat), (VarTypeFloat, OpLtFloat))
    , ((VarTypeString, VarTypeString), (VarTypeString, OpLtString))
    ]
translateExpression (ExprNeq lhs rhs) =
  translateExpression (ExprNot (ExprEq lhs rhs))
translateExpression (ExprGt lhs rhs) = translateExpression (ExprLt rhs lhs)
translateExpression (ExprLeq lhs rhs) =
  translateExpression (ExprNot (ExprGt lhs rhs))
translateExpression (ExprGeq lhs rhs) =
  translateExpression (ExprNot (ExprLt lhs rhs))
