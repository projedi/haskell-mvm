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
      , funs = programFunctions p
      }
    (bcFuns, finalEnv) = runTranslator (programFunctions p) initEnv

data Env = Env
  { currentRetType :: Maybe VarType
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
  { currentRetType = Nothing
  , lastLabel = LabelID (-1)
  , lastConst = ConstID (-1)
  , vars = IntMap.empty
  , funs = IntMap.empty
  , consts = IntMap.empty
  , foreignFuns = IntMap.empty
  , lastForeignFun = FunID 0
  }

introduceVariableInEnv :: Env -> VarDecl -> Env
introduceVariableInEnv env (VarDecl vtype (VarID v)) =
  let (Nothing, vars') =
        IntMap.insertLookupWithKey (\_ val _ -> val) v vtype (vars env)
  in env
     { vars = vars'
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

findVariable :: VarID -> Translator VarType
findVariable (VarID v) = do
  Just vtype <- (IntMap.lookup v . vars) <$> State.get
  pure vtype

introduceVariable :: VarDecl -> Translator VarID
introduceVariable vdecl@(VarDecl vtype v) = do
  env <- State.get
  let env' = introduceVariableInEnv env vdecl
  State.put env'
  addOp $ OpIntroVar v vtype
  pure v

findFunction :: FunID -> Translator FunctionDef
findFunction (FunID f) = do
  Just fdef <- (IntMap.lookup f . funs) <$> State.get
  pure fdef

translateFunctionBody :: FunctionDef -> Translator ()
translateFunctionBody f = do
  envBefore <- State.get
  State.put $
    envBefore
    { currentRetType = funDefRetType f
    }
  vs <- mapM introduceVariable $ funDefParams f
  forM_ vs (addOp . OpStore)
  translateStatement $ StatementBlock $ funDefBody f
  envAfter <- State.get
  State.put $
    envAfter
    { currentRetType = currentRetType envBefore
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
      actualType <- translateExpression expr
      _ <- convert actualType expectedType
      addOp OpReturn

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
  eType <- translateExpression e
  when (eType /= VarTypeInt) $ error "Type mismatch"
  addOp $ OpJumpIfZero labelAfterLoop
  translateBlock block
  addOp $ OpJump labelLoopBegin
  addOp $ OpLabel labelAfterLoop
translateStatement (StatementAssign v expr) = do
  eType <- translateExpression expr
  vType <- findVariable v
  _ <- convert eType vType
  addOp $ OpStore v
translateStatement (StatementIfElse e blockTrue blockFalse) = do
  labelElseBranch <- newLabel
  labelAfterIf <- newLabel
  eType <- translateExpression e
  when (eType /= VarTypeInt) $ error "Type mismatch"
  addOp $ OpJumpIfZero labelElseBranch
  translateBlock blockTrue
  addOp $ OpJump labelAfterIf
  addOp $ OpLabel labelElseBranch
  translateBlock blockFalse
  addOp $ OpLabel labelAfterIf
translateStatement (StatementReturn Nothing) = translateReturn
translateStatement (StatementReturn (Just expr)) = translateReturnWithValue expr

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

translateArgs :: [Expr] -> [VarType] -> Translator ()
translateArgs args types = do
  valsWithTypes <- mapM (localTranslate . translateExpression) args
  let vals = convertArgs valsWithTypes types
  forM_ (reverse vals) Writer.tell

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

printCall :: [Expr] -> Translator ()
printCall [] = pure ()
printCall args = do
  valsWithTypes <- mapM (localTranslate . translateExpression) args
  forM_ (reverse (map snd valsWithTypes)) Writer.tell
  let types = map fst valsWithTypes
  cid <- newConstant $ ValueString $ Right $ getPrintfDesc types
  addOp (OpPushString cid)
  f <- generateNewForeignFunction "printf" Nothing (VarTypeString : types)
  addOp $ OpForeignCall f

functionCall :: FunctionCall -> Translator (Maybe VarType)
functionCall (PrintCall args) = printCall args >> pure Nothing
functionCall (ForeignFunctionCall (FunID fid) args) = do
  Just f <- (IntMap.lookup fid . foreignFuns) <$> State.get
  translateArgs args $ foreignFunDeclParams f
  addOp $ OpForeignCall (foreignFunDeclName f)
  pure $ foreignFunDeclRetType f
functionCall (NativeFunctionCall fid args) = do
  FunctionDef {funDefRetType = rettype
              ,funDefParams = params} <- findFunction fid
  translateArgs args $ map (\(VarDecl t _) -> t) params
  addOp $ OpCall fid
  pure rettype

loadVar :: VarID -> Translator VarType
loadVar vid = do
  vtype <- findVariable vid
  addOp $ OpLoad vid
  pure vtype

localTranslate :: Translator a -> Translator (a, BytecodeFunction)
localTranslate m = Trans.lift $ Writer.runWriterT m

convertOp :: VarType -> VarType -> Maybe Op
convertOp VarTypeInt VarTypeInt = Nothing
convertOp VarTypeFloat VarTypeFloat = Nothing
convertOp VarTypeString VarTypeString = Nothing
convertOp VarTypeInt VarTypeFloat = Just OpIntToFloat
convertOp _ _ = error "Type mismatch"

convert :: VarType -> VarType -> Translator VarType
convert tFrom tTo =
  case convertOp tFrom tTo of
    Nothing -> pure tTo
    Just op -> addOpWithType op

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

addOpWithType :: Op -> Translator VarType
addOpWithType op = addOp op >> pure (opRetType op)

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

translateBinOp :: Expr
               -> Expr
               -> Map (VarType, VarType) (VarType, Op)
               -> Translator VarType
translateBinOp lhs rhs table = do
  rhsType <- translateExpression rhs
  (lhsType, code) <- localTranslate $ translateExpression lhs
  case Map.lookup (lhsType, rhsType) table of
    Nothing -> error "Type mismatch"
    Just (resType, op) -> do
      _ <- convert rhsType resType
      Writer.tell code
      _ <- convert lhsType resType
      addOpWithType op

translateExpression :: Expr -> Translator VarType
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
