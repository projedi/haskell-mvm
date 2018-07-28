module SyntaxLinearizer
  ( linearize
  ) where

import Control.Monad.State (State, runState)
import qualified Control.Monad.State as State
import qualified Control.Monad.Trans as Trans
import Control.Monad.Writer (WriterT, execWriterT)
import qualified Control.Monad.Writer as Writer

import qualified LinearSyntax
import qualified SimplifiedSyntax

linearize :: SimplifiedSyntax.Program -> LinearSyntax.Program
linearize p =
  LinearSyntax.Program
    { LinearSyntax.programFunctions = fs
    , LinearSyntax.programLibraries = SimplifiedSyntax.programLibraries p
    , LinearSyntax.programForeignFunctions =
        SimplifiedSyntax.programForeignFunctions p
    , LinearSyntax.programConstants = SimplifiedSyntax.programConstants p
    , LinearSyntax.programVariables = SimplifiedSyntax.programVariables p
    , LinearSyntax.programLastFunID = SimplifiedSyntax.programLastFunID p
    , LinearSyntax.programLastVarID = SimplifiedSyntax.programLastVarID p
    , LinearSyntax.programLastConstID = SimplifiedSyntax.programLastConstID p
    , LinearSyntax.programLastLabelID = lastLabelID finalEnv
    }
  where
    (fs, finalEnv) =
      runState (mapM linearizeFunctionDef $ SimplifiedSyntax.programFunctions p) $
      Env {lastLabelID = LinearSyntax.LabelID 0}

data Env = Env
  { lastLabelID :: LinearSyntax.LabelID
  }

type Linearizer = State Env

linearizeFunctionDef ::
     SimplifiedSyntax.FunctionDef -> Linearizer LinearSyntax.FunctionDef
linearizeFunctionDef f = do
  body <- linearizeBlock $ SimplifiedSyntax.funDefBody f
  pure $
    LinearSyntax.FunctionDef
      { LinearSyntax.funDefRetType = SimplifiedSyntax.funDefRetType f
      , LinearSyntax.funDefName = SimplifiedSyntax.funDefName f
      , LinearSyntax.funDefParams = SimplifiedSyntax.funDefParams f
      , LinearSyntax.funDefBody = body
      }

linearizeBlock :: SimplifiedSyntax.Block -> Linearizer LinearSyntax.Block
linearizeBlock b = do
  ss <-
    execWriterT (mapM_ linearizeStatement $ SimplifiedSyntax.blockStatements b)
  pure $ LinearSyntax.Block {LinearSyntax.blockStatements = ss}

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
linearizeStatement (SimplifiedSyntax.StatementBlock b) = do
  b' <- Trans.lift $ linearizeBlock b
  addStatement $ LinearSyntax.StatementBlock b'
linearizeStatement (SimplifiedSyntax.StatementVarAlloc v) =
  addStatement $ LinearSyntax.StatementVarAlloc v
linearizeStatement (SimplifiedSyntax.StatementFunctionCall fcall) = do
  fcall' <- linearizeFunctionCall fcall
  addStatement $ LinearSyntax.StatementFunctionCall fcall'
linearizeStatement (SimplifiedSyntax.StatementWhile e b) = do
  e' <- linearizeExpr e
  b' <- Trans.lift $ linearizeBlock b
  addStatement $ LinearSyntax.StatementWhile e' b'
linearizeStatement (SimplifiedSyntax.StatementAssign v e) = do
  e' <- linearizeExpr e
  addStatement $ LinearSyntax.StatementAssign v e'
linearizeStatement (SimplifiedSyntax.StatementAssignToPtr v e) = do
  e' <- linearizeExpr e
  addStatement $ LinearSyntax.StatementAssignToPtr v e'
linearizeStatement (SimplifiedSyntax.StatementIfElse e tb fb) = do
  e' <- linearizeExpr e
  elseLabel <- newLabel
  endLabel <- newLabel
  addStatement $ LinearSyntax.StatementJumpIfZero e' elseLabel
  tb' <- Trans.lift $ linearizeBlock tb
  addStatement $ LinearSyntax.StatementBlock tb'
  addStatement $ LinearSyntax.StatementJump endLabel
  addStatement $ LinearSyntax.StatementLabel elseLabel
  fb' <- Trans.lift $ linearizeBlock fb
  addStatement $ LinearSyntax.StatementBlock fb'
  addStatement $ LinearSyntax.StatementLabel endLabel
linearizeStatement (SimplifiedSyntax.StatementReturn Nothing) =
  addStatement $ LinearSyntax.StatementReturn Nothing
linearizeStatement (SimplifiedSyntax.StatementReturn (Just e)) = do
  e' <- linearizeExpr e
  addStatement $ LinearSyntax.StatementReturn (Just e')

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

linearizeExpr :: SimplifiedSyntax.Expr -> StatementLinearizer LinearSyntax.Expr
linearizeExpr (SimplifiedSyntax.ExprFunctionCall fcall) =
  LinearSyntax.ExprFunctionCall <$> linearizeFunctionCall fcall
linearizeExpr (SimplifiedSyntax.ExprVar t v) = pure $ LinearSyntax.ExprVar t v
linearizeExpr (SimplifiedSyntax.ExprDereference t v) =
  pure $ LinearSyntax.ExprDereference t v
linearizeExpr (SimplifiedSyntax.ExprAddressOf t v) =
  pure $ LinearSyntax.ExprAddressOf t v
linearizeExpr (SimplifiedSyntax.ExprConst t c) =
  pure $ LinearSyntax.ExprConst t c
linearizeExpr (SimplifiedSyntax.ExprBinOp op lhs rhs) =
  LinearSyntax.ExprBinOp op <$> linearizeExpr lhs <*> linearizeExpr rhs
linearizeExpr (SimplifiedSyntax.ExprUnOp op e) =
  LinearSyntax.ExprUnOp op <$> linearizeExpr e
