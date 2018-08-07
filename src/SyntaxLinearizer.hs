{-# LANGUAGE FlexibleContexts #-}

module SyntaxLinearizer
  ( linearize
  ) where

import Control.Monad.State (MonadState, State, runState)
import qualified Control.Monad.State as State
import Control.Monad.Writer (WriterT, execWriterT)
import qualified Control.Monad.Writer as Writer
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import qualified LinearSyntax
import qualified SimplifiedSyntax

linearize :: SimplifiedSyntax.Program -> LinearSyntax.Program
linearize p =
  LinearSyntax.Program
    { LinearSyntax.programFunctions = fs
    , LinearSyntax.programLibraries = SimplifiedSyntax.programLibraries p
    , LinearSyntax.programForeignFunctions =
        SimplifiedSyntax.programForeignFunctions p
    , LinearSyntax.programStrings = SimplifiedSyntax.programStrings p
    , LinearSyntax.programVariables = vars finalEnv
    , LinearSyntax.programLastFunID = SimplifiedSyntax.programLastFunID p
    , LinearSyntax.programLastVarID = lastVarID finalEnv
    , LinearSyntax.programLastStringID = SimplifiedSyntax.programLastStringID p
    , LinearSyntax.programLastLabelID = lastLabelID finalEnv
    }
  where
    (fs, finalEnv) =
      runState (mapM linearizeFunctionDef $ SimplifiedSyntax.programFunctions p) $
      Env
        { lastLabelID = LinearSyntax.LabelID 0
        , foundVariables = []
        , lastVarID = SimplifiedSyntax.programLastVarID p
        , vars = SimplifiedSyntax.programVariables p
        }

data Env = Env
  { lastLabelID :: LinearSyntax.LabelID
  , foundVariables :: [LinearSyntax.Var]
  , lastVarID :: LinearSyntax.VarID
  , vars :: IntMap LinearSyntax.VarType
  }

getVarType :: MonadState Env m => LinearSyntax.VarID -> m LinearSyntax.VarType
getVarType (LinearSyntax.VarID vid) = State.gets ((IntMap.! vid) . vars)

type Linearizer = State Env

linearizeFunctionDef ::
     SimplifiedSyntax.FunctionDef -> Linearizer LinearSyntax.FunctionDef
linearizeFunctionDef f = do
  State.modify $ \env -> env {foundVariables = []}
  body <- execWriterT $ linearizeBlock $ SimplifiedSyntax.funDefBody f
  locals <- State.gets foundVariables
  pure $
    LinearSyntax.FunctionDef
      { LinearSyntax.funDefRetType = SimplifiedSyntax.funDefRetType f
      , LinearSyntax.funDefName = SimplifiedSyntax.funDefName f
      , LinearSyntax.funDefParams =
          map varDeclToVar $ SimplifiedSyntax.funDefParams f
      , LinearSyntax.funDefLocals = locals
      , LinearSyntax.funDefBody = body
      }
  where
    varDeclToVar (SimplifiedSyntax.VarDecl t v) = LinearSyntax.Var v t

linearizeBlock :: SimplifiedSyntax.Block -> StatementLinearizer ()
linearizeBlock = mapM_ linearizeStatement . SimplifiedSyntax.blockStatements

type StatementLinearizer = WriterT [LinearSyntax.Statement] Linearizer

addStatement :: LinearSyntax.Statement -> StatementLinearizer ()
addStatement s = Writer.tell [s]

newLabel :: StatementLinearizer LinearSyntax.LabelID
newLabel = do
  nextLabelID <- State.gets (inc . lastLabelID)
  State.modify $ \env -> env {lastLabelID = nextLabelID}
  pure nextLabelID
  where
    inc (LinearSyntax.LabelID lid) = LinearSyntax.LabelID (lid + 1)

linearizeStatement :: SimplifiedSyntax.Statement -> StatementLinearizer ()
linearizeStatement (SimplifiedSyntax.StatementBlock b) = linearizeBlock b
linearizeStatement (SimplifiedSyntax.StatementVarAlloc v) = do
  t <- getVarType v
  State.modify $ \env ->
    env
      { foundVariables =
          LinearSyntax.Var {LinearSyntax.varName = v, LinearSyntax.varType = t} :
          foundVariables env
      }
linearizeStatement (SimplifiedSyntax.StatementFunctionCall fcall) = do
  fcall' <- linearizeFunctionCall fcall
  addStatement $ LinearSyntax.StatementFunctionCall fcall'
linearizeStatement (SimplifiedSyntax.StatementWhile e b) = do
  loopBegin <- newLabel
  loopEnd <- newLabel
  addStatement $ LinearSyntax.StatementLabel loopBegin
  v <- linearizeExpr e
  addStatement $ LinearSyntax.StatementJumpIfZero v loopEnd
  linearizeBlock b
  addStatement $ LinearSyntax.StatementJump loopBegin
  addStatement $ LinearSyntax.StatementLabel loopEnd
linearizeStatement (SimplifiedSyntax.StatementAssign v e) = do
  t <- getVarType v
  e' <- linearizeExprImpl e
  addStatement $ LinearSyntax.StatementAssign (LinearSyntax.Var v t) e'
linearizeStatement (SimplifiedSyntax.StatementAssignToPtr p e) = do
  t <- getVarType p
  v <- linearizeExpr e
  addStatement $ LinearSyntax.StatementAssignToPtr (LinearSyntax.Var p t) v
linearizeStatement (SimplifiedSyntax.StatementIfElse e tb fb) = do
  v <- linearizeExpr e
  elseLabel <- newLabel
  endLabel <- newLabel
  addStatement $ LinearSyntax.StatementJumpIfZero v elseLabel
  linearizeBlock tb
  addStatement $ LinearSyntax.StatementJump endLabel
  addStatement $ LinearSyntax.StatementLabel elseLabel
  linearizeBlock fb
  addStatement $ LinearSyntax.StatementLabel endLabel
linearizeStatement (SimplifiedSyntax.StatementReturn Nothing) =
  addStatement $ LinearSyntax.StatementReturn Nothing
linearizeStatement (SimplifiedSyntax.StatementReturn (Just e)) = do
  v <- linearizeExpr e
  addStatement $ LinearSyntax.StatementReturn (Just v)

linearizeFunctionCall ::
     SimplifiedSyntax.FunctionCall
  -> StatementLinearizer LinearSyntax.FunctionCall
linearizeFunctionCall fcall@SimplifiedSyntax.NativeFunctionCall {} = do
  args <- mapM linearizeExpr $ SimplifiedSyntax.nativeFunCallArgs fcall
  pure $
    LinearSyntax.NativeFunctionCall
      { LinearSyntax.nativeFunCallName =
          SimplifiedSyntax.nativeFunCallName fcall
      , LinearSyntax.nativeFunCallRetType =
          SimplifiedSyntax.nativeFunCallRetType fcall
      , LinearSyntax.nativeFunCallArgs = args
      }
linearizeFunctionCall fcall@SimplifiedSyntax.ForeignFunctionCall {} = do
  args <- mapM linearizeExpr $ SimplifiedSyntax.foreignFunCallArgs fcall
  pure $
    LinearSyntax.ForeignFunctionCall
      { LinearSyntax.foreignFunCallName =
          SimplifiedSyntax.foreignFunCallName fcall
      , LinearSyntax.foreignFunCallRetType =
          SimplifiedSyntax.foreignFunCallRetType fcall
      , LinearSyntax.foreignFunCallArgs = args
      }

introduceVariable ::
     LinearSyntax.VarType -> StatementLinearizer LinearSyntax.Var
introduceVariable vtype = do
  newVarID@(LinearSyntax.VarID vid) <- State.gets (inc . lastVarID)
  State.modify $ \env ->
    env
      { vars = IntMap.insert vid vtype $ vars env
      , lastVarID = newVarID
      , foundVariables =
          LinearSyntax.Var
            {LinearSyntax.varName = newVarID, LinearSyntax.varType = vtype} :
          foundVariables env
      }
  pure $
    LinearSyntax.Var
      {LinearSyntax.varType = vtype, LinearSyntax.varName = newVarID}
  where
    inc (LinearSyntax.VarID v) = LinearSyntax.VarID (v + 1)

linearizeExprImpl ::
     SimplifiedSyntax.Expr -> StatementLinearizer LinearSyntax.Expr
linearizeExprImpl (SimplifiedSyntax.ExprFunctionCall fcall) =
  LinearSyntax.ExprFunctionCall <$> linearizeFunctionCall fcall
linearizeExprImpl (SimplifiedSyntax.ExprVar t v) =
  pure $ LinearSyntax.ExprVar (LinearSyntax.Var v t)
linearizeExprImpl (SimplifiedSyntax.ExprDereference t v) =
  pure $
  LinearSyntax.ExprDereference (LinearSyntax.Var v (LinearSyntax.VarTypePtr t))
linearizeExprImpl (SimplifiedSyntax.ExprAddressOf (LinearSyntax.VarTypePtr t) v) =
  pure $ LinearSyntax.ExprAddressOf (LinearSyntax.Var v t)
linearizeExprImpl (SimplifiedSyntax.ExprAddressOf _ _) = error "Type mismatch"
linearizeExprImpl (SimplifiedSyntax.ExprConst c) =
  pure $ LinearSyntax.ExprConst c
linearizeExprImpl (SimplifiedSyntax.ExprBinOp op lhs rhs) = do
  lhsV <- linearizeExpr lhs
  rhsV <- linearizeExpr rhs
  pure $ LinearSyntax.ExprBinOp op lhsV rhsV
linearizeExprImpl (SimplifiedSyntax.ExprUnOp op e) = do
  v <- linearizeExpr e
  pure $ LinearSyntax.ExprUnOp op v

linearizeExpr :: SimplifiedSyntax.Expr -> StatementLinearizer LinearSyntax.Var
linearizeExpr e = do
  e' <- linearizeExprImpl e
  v <- introduceVariable (LinearSyntax.exprType e')
  addStatement $ LinearSyntax.StatementAssign v e'
  pure v
