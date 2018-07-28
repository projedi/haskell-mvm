module SyntaxLinearizer
  ( linearize
  ) where

import Control.Monad.State (State, runState)
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
    , LinearSyntax.programConstants = SimplifiedSyntax.programConstants p
    , LinearSyntax.programVariables = vars finalEnv
    , LinearSyntax.programLastFunID = SimplifiedSyntax.programLastFunID p
    , LinearSyntax.programLastVarID = lastVarID finalEnv
    , LinearSyntax.programLastConstID = SimplifiedSyntax.programLastConstID p
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
  , foundVariables :: [LinearSyntax.VarID]
  , lastVarID :: LinearSyntax.VarID
  , vars :: IntMap LinearSyntax.VarType
  }

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
      , LinearSyntax.funDefParams = SimplifiedSyntax.funDefParams f
      , LinearSyntax.funDefLocals = locals
      , LinearSyntax.funDefBody = body
      }

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
linearizeStatement (SimplifiedSyntax.StatementVarAlloc v) =
  State.modify $ \env -> env {foundVariables = v : foundVariables env}
linearizeStatement (SimplifiedSyntax.StatementFunctionCall fcall) = do
  fcall' <- linearizeFunctionCall fcall
  addStatement $ LinearSyntax.StatementFunctionCall fcall'
linearizeStatement (SimplifiedSyntax.StatementWhile e b) = do
  loopBegin <- newLabel
  loopEnd <- newLabel
  addStatement $ LinearSyntax.StatementLabel loopBegin
  e' <- linearizeExpr e
  v <- extractExprToNewVar e'
  addStatement $ LinearSyntax.StatementJumpIfZero v loopEnd
  linearizeBlock b
  addStatement $ LinearSyntax.StatementJump loopBegin
  addStatement $ LinearSyntax.StatementLabel loopEnd
linearizeStatement (SimplifiedSyntax.StatementAssign v e) = do
  e' <- linearizeExpr e
  addStatement $ LinearSyntax.StatementAssign v e'
linearizeStatement (SimplifiedSyntax.StatementAssignToPtr v e) = do
  e' <- linearizeExpr e
  v' <- extractExprToNewVar e'
  addStatement $ LinearSyntax.StatementAssignToPtr v v'
linearizeStatement (SimplifiedSyntax.StatementIfElse e tb fb) = do
  e' <- linearizeExpr e
  v <- extractExprToNewVar e'
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

introduceVariable ::
     LinearSyntax.VarType -> StatementLinearizer LinearSyntax.Var
introduceVariable vtype = do
  newVarID@(LinearSyntax.VarID vid) <- State.gets (inc . lastVarID)
  State.modify $ \env ->
    env
      { vars = IntMap.insert vid vtype $ vars env
      , lastVarID = newVarID
      , foundVariables = newVarID : foundVariables env
      }
  pure $
    LinearSyntax.Var
      {LinearSyntax.varType = vtype, LinearSyntax.varName = newVarID}
  where
    inc (LinearSyntax.VarID v) = LinearSyntax.VarID (v + 1)

extractExprToNewVar :: LinearSyntax.Expr -> StatementLinearizer LinearSyntax.Var
extractExprToNewVar e = do
  v <- introduceVariable (LinearSyntax.exprType e)
  addStatement $ LinearSyntax.StatementAssign (LinearSyntax.varName v) e
  pure v

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
