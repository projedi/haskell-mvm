{-# LANGUAGE FlexibleContexts #-}

module ASM
  ( avenge
  ) where

import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Reader as Reader
import Control.Monad.State (MonadState, State, runState)
import qualified Control.Monad.State as State
import Control.Monad.Writer (MonadWriter, WriterT, execWriterT)
import qualified Control.Monad.Writer as Writer
import Data.Int (Int64)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import qualified ASMSyntax
import CallingConvention (CallingConvention)
import qualified CallingConvention
import qualified LinearSyntax

avenge :: LinearSyntax.Program -> ASMSyntax.Program
avenge p =
  ASMSyntax.Program
    { ASMSyntax.programCode = ASMSyntax.FunctionDef code
    , ASMSyntax.programLibraries = LinearSyntax.programLibraries p
    , ASMSyntax.programForeignFunctions = LinearSyntax.programForeignFunctions p
    , ASMSyntax.programStrings = LinearSyntax.programStrings p
    , ASMSyntax.programLastFunID = LinearSyntax.programLastFunID p
    , ASMSyntax.programLastStringID = LinearSyntax.programLastStringID p
    , ASMSyntax.programLastLabelID = lastLabelID finalEnv
    }
  where
    (code, finalEnv) =
      runState (execWriterT (translateCode (LinearSyntax.programFunctions p))) $
      Env
        { varMap = IntMap.empty
        , varStack = []
        , lastLabelID = LinearSyntax.programLastLabelID p
        , funIdToLabelID = IntMap.empty
        }

data Var = Var
  { varType :: ASMSyntax.VarType
  , varDisplacement :: Int64 -- Displacement from RBP.
  }

data Env = Env
  { varMap :: IntMap Var
  , varStack :: [ASMSyntax.VarType]
  , lastLabelID :: ASMSyntax.LabelID
  , funIdToLabelID :: IntMap ASMSyntax.LabelID
  }

type ASM = WriterT [ASMSyntax.Statement] (State Env)

nextLabel :: MonadState Env m => m ASMSyntax.LabelID
nextLabel = do
  lbl <- State.gets (inc . lastLabelID)
  State.modify $ \env -> env {lastLabelID = lbl}
  pure lbl
  where
    inc (ASMSyntax.LabelID lid) = ASMSyntax.LabelID (lid + 1)

opRBP :: ASMSyntax.IntOperand
opRBP = ASMSyntax.IntOperandRegister ASMSyntax.VarTypeInt ASMSyntax.RegisterRBP

opRSP :: ASMSyntax.IntOperand
opRSP = ASMSyntax.IntOperandRegister ASMSyntax.VarTypeInt ASMSyntax.RegisterRSP

opRAX :: ASMSyntax.VarType -> ASMSyntax.IntOperand
opRAX t = ASMSyntax.IntOperandRegister t ASMSyntax.RegisterRAX

opRCX :: ASMSyntax.VarType -> ASMSyntax.IntOperand
opRCX t = ASMSyntax.IntOperandRegister t ASMSyntax.RegisterRCX

opRDX :: ASMSyntax.VarType -> ASMSyntax.IntOperand
opRDX t = ASMSyntax.IntOperandRegister t ASMSyntax.RegisterRDX

opXMM0 :: ASMSyntax.FloatOperand
opXMM0 = ASMSyntax.FloatOperandRegister ASMSyntax.RegisterXMM0

peekStack :: ASMSyntax.VarType -> ASMSyntax.IntOperand
peekStack t =
  ASMSyntax.IntOperandPointer
    ASMSyntax.Pointer
      { ASMSyntax.pointerType = t
      , ASMSyntax.pointerBase = Just ASMSyntax.RegisterRSP
      , ASMSyntax.pointerDisplacement = -1
      }

introduceVariable :: LinearSyntax.Var -> ASM Var
introduceVariable LinearSyntax.Var { LinearSyntax.varName = LinearSyntax.VarID vid
                                   , LinearSyntax.varType = t
                                   } = do
  d <- State.gets (fromIntegral . length . varStack)
  let var = Var {varType = t, varDisplacement = d}
  State.modify $ \env ->
    env
      {varMap = IntMap.insert vid var $ varMap env, varStack = t : varStack env}
  pure var

resolveVariable :: MonadState Env m => LinearSyntax.Var -> m Var
resolveVariable LinearSyntax.Var {LinearSyntax.varName = LinearSyntax.VarID vid} =
  State.gets ((IntMap.! vid) . varMap)

resolveVariableAsIntOperand ::
     MonadState Env m => LinearSyntax.Var -> m ASMSyntax.IntOperand
resolveVariableAsIntOperand v = do
  Var {varType = t, varDisplacement = d} <- resolveVariable v
  pure $
    ASMSyntax.IntOperandPointer
      ASMSyntax.Pointer
        { ASMSyntax.pointerType = t
        , ASMSyntax.pointerBase = Just ASMSyntax.RegisterRBP
        , ASMSyntax.pointerDisplacement = d
        }

resolveVariableAsFloatOperand ::
     MonadState Env m => LinearSyntax.Var -> m ASMSyntax.FloatOperand
resolveVariableAsFloatOperand v = do
  Var {varType = ASMSyntax.VarTypeFloat, varDisplacement = d} <-
    resolveVariable v
  pure $
    ASMSyntax.FloatOperandPointer
      ASMSyntax.Pointer
        { ASMSyntax.pointerType = ASMSyntax.VarTypeFloat
        , ASMSyntax.pointerBase = Just ASMSyntax.RegisterRBP
        , ASMSyntax.pointerDisplacement = d
        }

translateCode :: IntMap LinearSyntax.FunctionDef -> ASM ()
translateCode fs = do
  let fids = IntMap.keys fs
  let generateLabelForFunID fid = do
        l <- nextLabel
        pure (fid, l)
  fidLabelMap <- mapM generateLabelForFunID fids
  let funMap = IntMap.fromList fidLabelMap
  State.modify $ \env -> env {funIdToLabelID = funMap}
  addStatement $
    ASMSyntax.InstructionCALL $
    ASMSyntax.NativeFunctionCall
      {ASMSyntax.nativeFunCallName = funMap IntMap.! 0}
  addStatement $ ASMSyntax.InstructionRET
  mapM_ translateFunctionDef fs

translateFunctionDef :: LinearSyntax.FunctionDef -> ASM ()
translateFunctionDef fdef = do
  State.modify $ \env -> env {varStack = []}
  params <- mapM introduceVariable $ LinearSyntax.funDefParams fdef
  locals <- mapM introduceVariable $ LinearSyntax.funDefLocals fdef
  State.modify $ \env -> env {varStack = []}
  let cc =
        CallingConvention.computeCallingConvention
          CallingConvention.FunctionCall
            { CallingConvention.funRetType = LinearSyntax.funDefRetType fdef
            , CallingConvention.funArgTypes = map varType params
            }
  let retValue =
        (uncurry ASMSyntax.IntOperandRegister) <$>
        CallingConvention.funRetValue cc
  epilogueLbl <- nextLabel
  runASMStatement
    ConstEnv {retValueLocation = retValue, epilogueLabel = epilogueLbl} $ do
    let (LinearSyntax.FunID fid) = LinearSyntax.funDefName fdef
    funLbl <- State.gets ((IntMap.! fid) . funIdToLabelID)
    addStatement $ ASMSyntax.InstructionLabelledNOP funLbl
    let stackVars = params ++ locals
    addStatement $ ASMSyntax.StatementPushOnStack opRBP
    addStatement $ ASMSyntax.InstructionMOV opRBP (Left opRSP)
    mapM_
      (addStatement . ASMSyntax.StatementAllocateOnStack . varType)
      stackVars
    prepareArgsAtCall params cc
    mapM_ translateStatement $ LinearSyntax.funDefBody fdef
    addStatement $ ASMSyntax.InstructionLabelledNOP epilogueLbl
    mapM_ (addStatement . ASMSyntax.StatementPopFromStack . varType) $
      reverse stackVars
    addStatement $
      ASMSyntax.InstructionMOV opRBP (Left $ peekStack ASMSyntax.VarTypeInt)
    addStatement $ ASMSyntax.StatementPopFromStack ASMSyntax.VarTypeInt
    addStatement $ ASMSyntax.InstructionRET

type ASMStatement = ReaderT ConstEnv ASM

data ConstEnv = ConstEnv
  { retValueLocation :: Maybe ASMSyntax.IntOperand
  , epilogueLabel :: ASMSyntax.LabelID
  }

runASMStatement :: ConstEnv -> ASMStatement a -> ASM a
runASMStatement env m = runReaderT m env

addStatement ::
     MonadWriter [ASMSyntax.Statement] m => ASMSyntax.Statement -> m ()
addStatement s = Writer.tell [s]

translateStatement :: LinearSyntax.Statement -> ASMStatement ()
translateStatement (LinearSyntax.StatementFunctionCall fcall) = do
  _ <- translateFunctionCall fcall
  pure ()
translateStatement (LinearSyntax.StatementAssign v (LinearSyntax.ExprFunctionCall fcall)) = do
  v' <- resolveVariableAsIntOperand v
  cc <- translateFunctionCall fcall
  let (Just retValue) =
        (uncurry ASMSyntax.IntOperandRegister) <$>
        CallingConvention.funRetValue cc
  addStatement $ ASMSyntax.InstructionMOV v' (Left retValue)
translateStatement (LinearSyntax.StatementAssign v e) = do
  res <- translateExpr e
  case res of
    Left intOperand -> do
      v' <- resolveVariableAsIntOperand v
      addStatement $ ASMSyntax.InstructionMOV v' (Left intOperand)
    Right floatOperand -> do
      v' <- resolveVariableAsFloatOperand v
      addStatement $ ASMSyntax.StatementAssignFloat v' floatOperand
translateStatement (LinearSyntax.StatementAssignToPtr p v) = do
  p' <- resolveVariableAsIntOperand p
  addStatement $ ASMSyntax.InstructionMOV (opRAX ASMSyntax.VarTypeInt) (Left p')
  v' <- resolveVariableAsIntOperand v
  addStatement $
    ASMSyntax.InstructionMOV
      (ASMSyntax.IntOperandPointer
         ASMSyntax.Pointer
           { ASMSyntax.pointerType = ASMSyntax.intOperandType v'
           , ASMSyntax.pointerBase = Just ASMSyntax.RegisterRAX
           , ASMSyntax.pointerDisplacement = 0
           })
      (Left v')
translateStatement (LinearSyntax.StatementReturn Nothing) = do
  Nothing <- Reader.asks retValueLocation
  lbl <- Reader.asks epilogueLabel
  addStatement $ ASMSyntax.InstructionJMP lbl
translateStatement (LinearSyntax.StatementReturn (Just v)) = do
  v' <- resolveVariableAsIntOperand v
  Just rl <- Reader.asks retValueLocation
  addStatement $ ASMSyntax.InstructionMOV rl (Left v')
  lbl <- Reader.asks epilogueLabel
  addStatement $ ASMSyntax.InstructionJMP lbl
translateStatement (LinearSyntax.StatementLabel l) =
  addStatement $ ASMSyntax.InstructionLabelledNOP l
translateStatement (LinearSyntax.StatementJump l) =
  addStatement $ ASMSyntax.InstructionJMP l
translateStatement (LinearSyntax.StatementJumpIfZero v l) = do
  v' <- resolveVariableAsIntOperand v
  compareIntToZero v'
  addStatement $ ASMSyntax.InstructionJZ l

prepareArgsForCall ::
     CallingConvention -> [ASMSyntax.IntOperand] -> ASMStatement ()
prepareArgsForCall cc args = do
  mapM_
    (addStatement . ASMSyntax.StatementAllocateOnStack)
    (CallingConvention.funStackToAllocate cc)
  mapM_ go (zip args (CallingConvention.funArgValues cc))
  addStatement $
    ASMSyntax.InstructionMOV (opRAX ASMSyntax.VarTypeInt) $
    Right $
    ASMSyntax.ImmediateInt
      (fromIntegral $ CallingConvention.funFloatRegistersUsed cc)
  where
    go ::
         (ASMSyntax.IntOperand, CallingConvention.ArgLocation)
      -> ASMStatement ()
    go (arg, CallingConvention.ArgLocationRegister _ r) =
      addStatement $
      ASMSyntax.InstructionMOV
        (ASMSyntax.IntOperandRegister (ASMSyntax.intOperandType arg) r)
        (Left arg)
    go (arg, CallingConvention.ArgLocationStack t d) =
      addStatement $
      ASMSyntax.InstructionMOV
        (ASMSyntax.IntOperandPointer
           ASMSyntax.Pointer
             { ASMSyntax.pointerType = t
             , ASMSyntax.pointerBase = Just ASMSyntax.RegisterRSP
             , ASMSyntax.pointerDisplacement = -(d + 1)
             })
        (Left arg)

cleanStackAfterCall :: CallingConvention -> ASMStatement ()
cleanStackAfterCall cc =
  mapM_
    (addStatement . ASMSyntax.StatementPopFromStack)
    (reverse $ CallingConvention.funStackToAllocate cc)

prepareArgsAtCall :: [Var] -> CallingConvention -> ASMStatement ()
prepareArgsAtCall params cc = do
  let vals = map go (CallingConvention.funArgValues cc)
  generateAssignments params vals
  where
    go :: CallingConvention.ArgLocation -> ASMSyntax.IntOperand
    go (CallingConvention.ArgLocationRegister t r) =
      ASMSyntax.IntOperandRegister t r
    go (CallingConvention.ArgLocationStack t d) =
      ASMSyntax.IntOperandPointer
        ASMSyntax.Pointer
          { ASMSyntax.pointerType = t
          , ASMSyntax.pointerBase = Just ASMSyntax.RegisterRBP
          , ASMSyntax.pointerDisplacement = -(d + 3)
          }
    generateAssignments :: [Var] -> [ASMSyntax.IntOperand] -> ASMStatement ()
    generateAssignments [] [] = pure ()
    generateAssignments (Var {varType = t, varDisplacement = d}:vs) (val:vals) = do
      addStatement $
        ASMSyntax.InstructionMOV
          (ASMSyntax.IntOperandPointer
             ASMSyntax.Pointer
               { ASMSyntax.pointerType = t
               , ASMSyntax.pointerBase = Just ASMSyntax.RegisterRBP
               , ASMSyntax.pointerDisplacement = d
               })
          (Left val)
      generateAssignments vs vals
    generateAssignments _ _ = error "Type mismatch"

translateFunctionCall ::
     LinearSyntax.FunctionCall -> ASMStatement CallingConvention
translateFunctionCall fcall@LinearSyntax.NativeFunctionCall {} = do
  args <-
    mapM resolveVariableAsIntOperand $ LinearSyntax.nativeFunCallArgs fcall
  let cc =
        CallingConvention.computeCallingConvention
          CallingConvention.FunctionCall
            { CallingConvention.funRetType =
                LinearSyntax.nativeFunCallRetType fcall
            , CallingConvention.funArgTypes = map ASMSyntax.intOperandType args
            }
  prepareArgsForCall cc args
  let (LinearSyntax.FunID fid) = LinearSyntax.nativeFunCallName fcall
  flbl <- State.gets ((IntMap.! fid) . funIdToLabelID)
  addStatement $
    ASMSyntax.InstructionCALL $
    ASMSyntax.NativeFunctionCall {ASMSyntax.nativeFunCallName = flbl}
  cleanStackAfterCall cc
  pure cc
translateFunctionCall fcall@LinearSyntax.ForeignFunctionCall {} = do
  args <-
    mapM resolveVariableAsIntOperand $ LinearSyntax.foreignFunCallArgs fcall
  let cc =
        CallingConvention.computeCallingConvention
          CallingConvention.FunctionCall
            { CallingConvention.funRetType =
                LinearSyntax.foreignFunCallRetType fcall
            , CallingConvention.funArgTypes = map ASMSyntax.intOperandType args
            }
  prepareArgsForCall cc args
  addStatement $
    ASMSyntax.InstructionCALL $
    ASMSyntax.ForeignFunctionCall
      { ASMSyntax.foreignFunCallName = LinearSyntax.foreignFunCallName fcall
      , ASMSyntax.foreignFunCallRetType =
          LinearSyntax.foreignFunCallRetType fcall
      , ASMSyntax.foreignFunCallArgTypes = map ASMSyntax.intOperandType args
      }
  cleanStackAfterCall cc
  pure cc

type SomeOperand = Either ASMSyntax.IntOperand ASMSyntax.FloatOperand

translateExpr :: LinearSyntax.Expr -> ASMStatement SomeOperand
translateExpr (LinearSyntax.ExprFunctionCall _) =
  error "Must've been handled in translateStatement"
translateExpr (LinearSyntax.ExprVar v) = do
  v' <- resolveVariableAsIntOperand v
  let res = opRAX (ASMSyntax.intOperandType v')
  addStatement $ ASMSyntax.InstructionMOV res (Left v')
  pure $ Left res
translateExpr (LinearSyntax.ExprDereference v) = do
  v' <- resolveVariableAsIntOperand v
  addStatement $
    ASMSyntax.InstructionMOV (opRCX (ASMSyntax.intOperandType v')) (Left v')
  let res = opRAX (ASMSyntax.intOperandType v')
  addStatement $
    ASMSyntax.InstructionMOV
      res
      (Left $
       ASMSyntax.IntOperandPointer
         ASMSyntax.Pointer
           { ASMSyntax.pointerBase = Just ASMSyntax.RegisterRCX
           , ASMSyntax.pointerDisplacement = 0
           , ASMSyntax.pointerType = ASMSyntax.intOperandType v'
           })
  pure $ Left res
translateExpr (LinearSyntax.ExprAddressOf v) = do
  Var {varDisplacement = d} <- resolveVariable v
  addStatement $
    ASMSyntax.InstructionMOV
      (opRCX ASMSyntax.VarTypeInt)
      (Right $ ASMSyntax.ImmediateInt d)
  let rax = opRAX ASMSyntax.VarTypeInt
  addStatement $ ASMSyntax.InstructionMOV rax (Left opRBP)
  addStatement $ ASMSyntax.InstructionADD rax (opRCX ASMSyntax.VarTypeInt)
  pure $ Left rax
translateExpr (LinearSyntax.ExprConst c) = do
  let res = opRAX (ASMSyntax.immediateType c)
  addStatement $ ASMSyntax.InstructionMOV res (Right c)
  pure $ Left res
translateExpr (LinearSyntax.ExprBinOp op lhs rhs) = translateBinOp op lhs rhs
translateExpr (LinearSyntax.ExprUnOp op v) = translateUnOp op v

translateUnOp ::
     LinearSyntax.UnOp -> LinearSyntax.Var -> ASMStatement SomeOperand
translateUnOp LinearSyntax.UnNeg v =
  case (LinearSyntax.varType v) of
    ASMSyntax.VarTypeInt -> do
      v' <- resolveVariableAsIntOperand v
      let res = opRAX ASMSyntax.VarTypeInt
      addStatement $ ASMSyntax.InstructionMOV res (Left v')
      addStatement $ ASMSyntax.InstructionNEG res
      pure $ Left res
    ASMSyntax.VarTypeFloat -> do
      v' <- resolveVariableAsFloatOperand v
      let res = opXMM0
      addStatement $ ASMSyntax.StatementNegFloat v'
      pure $ Right res
    _ -> error "Type mismatch"
translateUnOp LinearSyntax.UnNot v = do
  v' <- resolveVariableAsIntOperand v
  compareIntToZero v'
  let res = opRAX ASMSyntax.VarTypeInt
  addStatement $ ASMSyntax.InstructionSetZ res
  pure $ Left res
translateUnOp LinearSyntax.UnIntToFloat v = do
  v' <- resolveVariableAsIntOperand v
  let res = opXMM0
  addStatement $ ASMSyntax.StatementIntToFloat v'
  pure $ Right res

translateBinOp ::
     LinearSyntax.BinOp
  -> LinearSyntax.Var
  -> LinearSyntax.Var
  -> ASMStatement SomeOperand
translateBinOp LinearSyntax.BinPlus lhs rhs =
  case (LinearSyntax.varType lhs, LinearSyntax.varType rhs) of
    (ASMSyntax.VarTypeInt, ASMSyntax.VarTypeInt) -> do
      lhs' <- resolveVariableAsIntOperand lhs
      rhs' <- resolveVariableAsIntOperand rhs
      let res = opRAX ASMSyntax.VarTypeInt
      addStatement $ ASMSyntax.InstructionMOV res (Left lhs')
      addStatement $ ASMSyntax.InstructionADD res rhs'
      pure $ Left res
    (ASMSyntax.VarTypeFloat, ASMSyntax.VarTypeFloat) -> do
      lhs' <- resolveVariableAsFloatOperand lhs
      rhs' <- resolveVariableAsFloatOperand rhs
      let res = opXMM0
      addStatement $ ASMSyntax.StatementBinOp ASMSyntax.BinPlusFloat lhs' rhs'
      pure $ Right res
    _ -> error "Type mismatch"
translateBinOp LinearSyntax.BinMinus lhs rhs =
  case (LinearSyntax.varType lhs, LinearSyntax.varType rhs) of
    (ASMSyntax.VarTypeInt, ASMSyntax.VarTypeInt) -> do
      lhs' <- resolveVariableAsIntOperand lhs
      rhs' <- resolveVariableAsIntOperand rhs
      let res = opRAX ASMSyntax.VarTypeInt
      addStatement $ ASMSyntax.InstructionMOV res (Left lhs')
      addStatement $ ASMSyntax.InstructionSUB res rhs'
      pure $ Left res
    (ASMSyntax.VarTypeFloat, ASMSyntax.VarTypeFloat) -> do
      lhs' <- resolveVariableAsFloatOperand lhs
      rhs' <- resolveVariableAsFloatOperand rhs
      let res = opXMM0
      addStatement $ ASMSyntax.StatementBinOp ASMSyntax.BinMinusFloat lhs' rhs'
      pure $ Right res
    _ -> error "Type mismatch"
translateBinOp LinearSyntax.BinTimes lhs rhs =
  case (LinearSyntax.varType lhs, LinearSyntax.varType rhs) of
    (ASMSyntax.VarTypeInt, ASMSyntax.VarTypeInt) -> do
      lhs' <- resolveVariableAsIntOperand lhs
      rhs' <- resolveVariableAsIntOperand rhs
      let res = opRAX ASMSyntax.VarTypeInt
      addStatement $ ASMSyntax.InstructionMOV res (Left lhs')
      addStatement $ ASMSyntax.InstructionIMUL res rhs'
      pure $ Left res
    (ASMSyntax.VarTypeFloat, ASMSyntax.VarTypeFloat) -> do
      lhs' <- resolveVariableAsFloatOperand lhs
      rhs' <- resolveVariableAsFloatOperand rhs
      let res = opXMM0
      addStatement $ ASMSyntax.StatementBinOp ASMSyntax.BinTimesFloat lhs' rhs'
      pure $ Right res
    _ -> error "Type mismatch"
translateBinOp LinearSyntax.BinDiv lhs rhs =
  case (LinearSyntax.varType lhs, LinearSyntax.varType rhs) of
    (ASMSyntax.VarTypeInt, ASMSyntax.VarTypeInt) -> do
      lhs' <- resolveVariableAsIntOperand lhs
      rhs' <- resolveVariableAsIntOperand rhs
      let res = opRAX ASMSyntax.VarTypeInt
      addStatement $ ASMSyntax.InstructionMOV res (Left lhs')
      addStatement $ ASMSyntax.InstructionCQO
      addStatement $ ASMSyntax.InstructionIDIV rhs'
      pure $ Left res
    (ASMSyntax.VarTypeFloat, ASMSyntax.VarTypeFloat) -> do
      lhs' <- resolveVariableAsFloatOperand lhs
      rhs' <- resolveVariableAsFloatOperand rhs
      let res = opXMM0
      addStatement $ ASMSyntax.StatementBinOp ASMSyntax.BinDivFloat lhs' rhs'
      pure $ Right res
    _ -> error "Type mismatch"
translateBinOp LinearSyntax.BinMod lhs rhs =
  case (LinearSyntax.varType lhs, LinearSyntax.varType rhs) of
    (ASMSyntax.VarTypeInt, ASMSyntax.VarTypeInt) -> do
      lhs' <- resolveVariableAsIntOperand lhs
      rhs' <- resolveVariableAsIntOperand rhs
      let res = opRAX ASMSyntax.VarTypeInt
      addStatement $ ASMSyntax.InstructionMOV res (Left lhs')
      addStatement $ ASMSyntax.InstructionCQO
      addStatement $ ASMSyntax.InstructionIDIV rhs'
      addStatement $
        ASMSyntax.InstructionMOV res (Left $ opRDX ASMSyntax.VarTypeInt)
      pure $ Left res
    _ -> error "Type mismatch"
translateBinOp LinearSyntax.BinBitAnd lhs rhs = do
  lhs' <- resolveVariableAsIntOperand lhs
  rhs' <- resolveVariableAsIntOperand rhs
  let res = opRAX ASMSyntax.VarTypeInt
  addStatement $ ASMSyntax.InstructionMOV res (Left lhs')
  addStatement $ ASMSyntax.InstructionAND res rhs'
  pure $ Left res
translateBinOp LinearSyntax.BinBitOr lhs rhs = do
  lhs' <- resolveVariableAsIntOperand lhs
  rhs' <- resolveVariableAsIntOperand rhs
  let res = opRAX ASMSyntax.VarTypeInt
  addStatement $ ASMSyntax.InstructionMOV res (Left lhs')
  addStatement $ ASMSyntax.InstructionOR res rhs'
  pure $ Left res
translateBinOp LinearSyntax.BinBitXor lhs rhs = do
  lhs' <- resolveVariableAsIntOperand lhs
  rhs' <- resolveVariableAsIntOperand rhs
  let res = opRAX ASMSyntax.VarTypeInt
  addStatement $ ASMSyntax.InstructionMOV res (Left lhs')
  addStatement $ ASMSyntax.InstructionXOR res rhs'
  pure $ Left res
translateBinOp LinearSyntax.BinAnd lhs rhs = do
  lhs' <- resolveVariableAsIntOperand lhs
  rhs' <- resolveVariableAsIntOperand rhs
  compareIntToZero lhs'
  let lhs'' = opRCX ASMSyntax.VarTypeInt
  addStatement $ ASMSyntax.InstructionSetNZ lhs''
  compareIntToZero rhs'
  let rhs'' = opRDX ASMSyntax.VarTypeInt
  addStatement $ ASMSyntax.InstructionSetNZ rhs''
  let res = opRAX ASMSyntax.VarTypeInt
  addStatement $ ASMSyntax.InstructionMOV res (Left lhs'')
  addStatement $ ASMSyntax.InstructionAND res rhs''
  pure $ Left res
translateBinOp LinearSyntax.BinOr lhs rhs = do
  lhs' <- resolveVariableAsIntOperand lhs
  rhs' <- resolveVariableAsIntOperand rhs
  compareIntToZero lhs'
  let lhs'' = opRCX ASMSyntax.VarTypeInt
  addStatement $ ASMSyntax.InstructionSetNZ lhs''
  compareIntToZero rhs'
  let rhs'' = opRDX ASMSyntax.VarTypeInt
  addStatement $ ASMSyntax.InstructionSetNZ rhs''
  let res = opRAX ASMSyntax.VarTypeInt
  addStatement $ ASMSyntax.InstructionMOV res (Left lhs'')
  addStatement $ ASMSyntax.InstructionOR res rhs''
  pure $ Left res
translateBinOp LinearSyntax.BinEq lhs rhs =
  case (LinearSyntax.varType lhs, LinearSyntax.varType rhs) of
    (ASMSyntax.VarTypeInt, ASMSyntax.VarTypeInt) -> do
      lhs' <- resolveVariableAsIntOperand lhs
      rhs' <- resolveVariableAsIntOperand rhs
      addStatement $ ASMSyntax.InstructionCMP lhs' rhs'
      let res = opRAX ASMSyntax.VarTypeInt
      addStatement $ ASMSyntax.InstructionSetZ res
      pure $ Left res
    (ASMSyntax.VarTypeFloat, ASMSyntax.VarTypeFloat) -> do
      lhs' <- resolveVariableAsFloatOperand lhs
      rhs' <- resolveVariableAsFloatOperand rhs
      let res = opRAX ASMSyntax.VarTypeInt
      addStatement $ ASMSyntax.StatementEqFloat lhs' rhs'
      pure $ Left res
    _ -> error "Type mismatch"
translateBinOp LinearSyntax.BinLt lhs rhs =
  case (LinearSyntax.varType lhs, LinearSyntax.varType rhs) of
    (ASMSyntax.VarTypeInt, ASMSyntax.VarTypeInt) -> do
      lhs' <- resolveVariableAsIntOperand lhs
      rhs' <- resolveVariableAsIntOperand rhs
      addStatement $ ASMSyntax.InstructionCMP lhs' rhs'
      let res = opRAX ASMSyntax.VarTypeInt
      addStatement $ ASMSyntax.InstructionSetS res
      pure $ Left res
    (ASMSyntax.VarTypeFloat, ASMSyntax.VarTypeFloat) -> do
      lhs' <- resolveVariableAsFloatOperand lhs
      rhs' <- resolveVariableAsFloatOperand rhs
      let res = opRAX ASMSyntax.VarTypeInt
      addStatement $ ASMSyntax.StatementLtFloat lhs' rhs'
      pure $ Left res
    _ -> error "Type mismatch"

-- Compare int operand to zero and set EFLAGS.
compareIntToZero :: ASMSyntax.IntOperand -> ASMStatement ()
compareIntToZero v = do
  addStatement $
    ASMSyntax.InstructionMOV
      (opRAX ASMSyntax.VarTypeInt)
      (Right $ ASMSyntax.ImmediateInt 0)
  addStatement $ ASMSyntax.InstructionCMP v (opRAX ASMSyntax.VarTypeInt)
