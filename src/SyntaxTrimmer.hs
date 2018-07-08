module SyntaxTrimmer
  ( trimAndSetCaptures
  ) where

import Control.Monad
import Control.Monad.Reader (Reader, runReader)
import qualified Control.Monad.Reader as Reader
import Control.Monad.State (State, execState)
import qualified Control.Monad.State as State
import Control.Monad.Writer (Writer, execWriter)
import qualified Control.Monad.Writer as Writer
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Maybe

import Syntax

trimAndSetCaptures :: Program -> Program
trimAndSetCaptures p = setCaptures usages $ trim usages p
  where
    usages = collectUsages p

data Usage = Usage
  { varUsage :: IntSet
  , localVarUsage :: IntSet
  , funUsage :: IntSet
  , localFunUsage :: IntSet
  , foreignFunUsage :: IntSet
  }

instance Monoid Usage where
  mempty =
    Usage
      { varUsage = IntSet.empty
      , localVarUsage = IntSet.empty
      , funUsage = IntSet.empty
      , localFunUsage = IntSet.empty
      , foreignFunUsage = IntSet.empty
      }
  lhs `mappend` rhs =
    Usage
      { varUsage = varUsage lhs `mappend` varUsage rhs
      , localVarUsage = localVarUsage lhs `mappend` localVarUsage rhs
      , funUsage = funUsage lhs `mappend` funUsage rhs
      , localFunUsage = localFunUsage lhs `mappend` localFunUsage rhs
      , foreignFunUsage = foreignFunUsage lhs `mappend` foreignFunUsage rhs
      }

getCaptures :: Usage -> IntSet
getCaptures usage = IntSet.difference (varUsage usage) (localVarUsage usage)

getCapturesAsList :: Usage -> [VarID]
getCapturesAsList = map VarID . IntSet.toList . getCaptures

collectUsages :: Program -> IntMap Usage
collectUsages p = collectTransitiveUsages $ execState (go 0) IntMap.empty
  where
    go :: Int -> State (IntMap Usage) ()
    go fid = do
      visited <- State.gets (IntMap.lookup fid)
      case visited of
        Just _ -> pure ()
        Nothing -> do
          let Just fdef = IntMap.lookup fid $ programFunctions p
              usage = execWriter $ collectUsageInFunction fdef
          State.modify $ IntMap.insert fid usage
          forM_ (IntSet.toList $ localFunUsage usage) go

collectTransitiveUsages :: IntMap Usage -> IntMap Usage
collectTransitiveUsages usages = iterate goM usages !! IntMap.size usages
  where
    goM :: IntMap Usage -> IntMap Usage
    goM u = fst $ execState (go 0) (u, IntSet.empty)
    go :: Int -> State (IntMap Usage, IntSet) ()
    go fid = do
      visited <- State.gets (IntSet.member fid . snd)
      unless visited $ do
        State.modify $ \(us, vs) -> (us, IntSet.insert fid vs)
        usage <- currentUsage fid
        forM_ (IntSet.toList $ localFunUsage usage) go
        transitiveUsage <-
          mconcat <$>
          mapM
            (fmap capturesOnly . currentUsage)
            (IntSet.toList $ localFunUsage usage)
        State.modify $ \(us, vs) ->
          ( IntMap.update
              (Just . updateUsageWithTransitive transitiveUsage)
              fid
              us
          , vs)
    currentUsage :: Int -> State (IntMap Usage, IntSet) Usage
    currentUsage fid = State.gets (fromJust . IntMap.lookup fid . fst)
    capturesOnly u = u {varUsage = getCaptures u}
    updateUsageWithTransitive tu u =
      u `mappend`
      tu {localVarUsage = localVarUsage u, localFunUsage = localFunUsage u}

type CollectUsage = Writer Usage

markLocalVariable :: VarDecl -> CollectUsage ()
markLocalVariable (VarDecl _ (VarID v)) =
  Writer.tell $ mempty {localVarUsage = IntSet.singleton v}

markVariableAsUsed :: VarID -> CollectUsage ()
markVariableAsUsed (VarID v) =
  Writer.tell $ mempty {varUsage = IntSet.singleton v}

markFunctionAsUsed :: FunID -> CollectUsage ()
markFunctionAsUsed (FunID f) =
  Writer.tell $
  mempty {funUsage = IntSet.singleton f, localFunUsage = IntSet.singleton f}

markForeignFunctionAsUsed :: FunID -> CollectUsage ()
markForeignFunctionAsUsed (FunID f) =
  Writer.tell $ mempty {foreignFunUsage = IntSet.singleton f}

collectUsageInFunction :: FunctionDef -> CollectUsage ()
collectUsageInFunction fdef = do
  forM_ (funDefParams fdef) markLocalVariable
  forM_ (funDefLocals fdef) markLocalVariable
  collectUsageInBlock (funDefBody fdef)

collectUsageInBlock :: Block -> CollectUsage ()
collectUsageInBlock block =
  forM_ (blockStatements block) collectUsageInStatement

collectUsageInStatement :: Statement -> CollectUsage ()
collectUsageInStatement (StatementBlock b) = collectUsageInBlock b
collectUsageInStatement (StatementFunctionCall fcall) =
  collectUsageInFunctionCall fcall
collectUsageInStatement (StatementWhile e b) = do
  collectUsageInExpr e
  collectUsageInBlock b
collectUsageInStatement (StatementAssign v e) = do
  markVariableAsUsed v
  collectUsageInExpr e
collectUsageInStatement (StatementIfElse e bt bf) = do
  collectUsageInExpr e
  collectUsageInBlock bt
  collectUsageInBlock bf
collectUsageInStatement (StatementReturn Nothing) = pure ()
collectUsageInStatement (StatementReturn (Just e)) = collectUsageInExpr e

collectUsageInFunctionCall :: FunctionCall -> CollectUsage ()
collectUsageInFunctionCall f@NativeFunctionCall {} = do
  markFunctionAsUsed (nativeFunCallName f)
  forM_ (nativeFunCallArgs f) collectUsageInExpr
collectUsageInFunctionCall f@ForeignFunctionCall {} = do
  markForeignFunctionAsUsed (foreignFunCallName f)
  forM_ (foreignFunCallArgs f) collectUsageInExpr
collectUsageInFunctionCall (PrintCall args) = forM_ args collectUsageInExpr

collectUsageInExpr :: Expr -> CollectUsage ()
collectUsageInExpr (ExprFunctionCall fcall) = collectUsageInFunctionCall fcall
collectUsageInExpr (ExprVar _ v) = markVariableAsUsed v
collectUsageInExpr (ExprConst _ _) = pure ()
collectUsageInExpr (ExprBinOp _ lhs rhs) = do
  collectUsageInExpr lhs
  collectUsageInExpr rhs
collectUsageInExpr (ExprUnOp _ e) = collectUsageInExpr e
collectUsageInExpr _ = undefined -- TODO: Remove when pattern synonyms have COMPLETE pragma.

trim :: IntMap Usage -> Program -> Program
trim usages p = trimVariables usages $ trimFunctions fullUsage p
  where
    Just usageOfMain = IntMap.lookup 0 usages
    fullUsage = usageOfMain `mappend` mempty {funUsage = IntSet.singleton 0}

trimFunctions :: Usage -> Program -> Program
trimFunctions usage p =
  p
    { programFunctions = keepKeys (funUsage usage) (programFunctions p)
    , programForeignFunctions =
        keepKeys (foreignFunUsage usage) (programForeignFunctions p)
    }
  where
    keepKeys ks = IntMap.filterWithKey (\k _ -> k `IntSet.member` ks)

trimVariables :: IntMap Usage -> Program -> Program
trimVariables usages p =
  p {programFunctions = IntMap.mapWithKey go $ programFunctions p}
  where
    go fid f =
      let Just usage = IntMap.lookup fid usages
       in trimVariablesInFunction usage f

trimVariablesInFunction :: Usage -> FunctionDef -> FunctionDef
trimVariablesInFunction usage f =
  f
    { funDefBody = trimVariablesInBlock usage $ funDefBody f
    , funDefLocals = trimVariableList usage $ funDefLocals f
    }

trimVariablesInBlock :: Usage -> Block -> Block
trimVariablesInBlock usage block =
  block
    {blockStatements = trimVariablesInStatement usage <$> blockStatements block}

trimVariableList :: Usage -> [VarDecl] -> [VarDecl]
trimVariableList usage = filter go
  where
    go (VarDecl _ (VarID vid)) = IntSet.member vid (varUsage usage)

trimVariablesInStatement :: Usage -> Statement -> Statement
trimVariablesInStatement usage (StatementBlock b) =
  StatementBlock $ trimVariablesInBlock usage b
trimVariablesInStatement _ f@(StatementFunctionCall _) = f
trimVariablesInStatement usage (StatementWhile e b) =
  StatementWhile e $ trimVariablesInBlock usage b
trimVariablesInStatement _ f@(StatementAssign _ _) = f
trimVariablesInStatement usage (StatementIfElse e bt bf) =
  StatementIfElse
    e
    (trimVariablesInBlock usage bt)
    (trimVariablesInBlock usage bf)
trimVariablesInStatement _ f@(StatementReturn _) = f

setCaptures :: IntMap Usage -> Program -> Program
setCaptures usage p = p {programFunctions = go <$> programFunctions p}
  where
    go fdef =
      let FunID fid = funDefName fdef
          Just fusage = IntMap.lookup fid usage
       in fdef
            { funDefCaptures = getCapturesAsList fusage
            , funDefBody =
                runReader (setCapturesInBlock (funDefBody fdef)) usage
            }

type SetCaptures = Reader (IntMap Usage)

setCapturesInBlock :: Block -> SetCaptures Block
setCapturesInBlock block = do
  stmts <- mapM setCapturesInStatement (blockStatements block)
  pure $ block {blockStatements = stmts}

setCapturesInStatement :: Statement -> SetCaptures Statement
setCapturesInStatement (StatementBlock block) =
  StatementBlock <$> setCapturesInBlock block
setCapturesInStatement (StatementFunctionCall fcall) =
  StatementFunctionCall <$> setCapturesInFunctionCall fcall
setCapturesInStatement (StatementWhile e b) =
  StatementWhile <$> setCapturesInExpr e <*> setCapturesInBlock b
setCapturesInStatement (StatementAssign v e) =
  StatementAssign v <$> setCapturesInExpr e
setCapturesInStatement (StatementIfElse e bt bf) =
  StatementIfElse <$> setCapturesInExpr e <*> setCapturesInBlock bt <*>
  setCapturesInBlock bf
setCapturesInStatement (StatementReturn Nothing) =
  pure $ StatementReturn Nothing
setCapturesInStatement (StatementReturn (Just e)) =
  StatementReturn . Just <$> setCapturesInExpr e

setCapturesInFunctionCall :: FunctionCall -> SetCaptures FunctionCall
setCapturesInFunctionCall f@ForeignFunctionCall {} = do
  args <- mapM setCapturesInExpr (foreignFunCallArgs f)
  pure $ f {foreignFunCallArgs = args}
setCapturesInFunctionCall (PrintCall args) = do
  args' <- mapM setCapturesInExpr args
  pure $ PrintCall args'
setCapturesInFunctionCall f@NativeFunctionCall {nativeFunCallName = FunID fid} = do
  args <- mapM setCapturesInExpr (nativeFunCallArgs f)
  Just fusage <- Reader.asks (IntMap.lookup fid)
  pure $
    f
      { nativeFunCallCaptures = getCapturesAsList fusage
      , nativeFunCallArgs = args
      }

setCapturesInExpr :: Expr -> SetCaptures Expr
setCapturesInExpr (ExprFunctionCall fcall) =
  ExprFunctionCall <$> setCapturesInFunctionCall fcall
setCapturesInExpr (ExprVar t v) = pure $ ExprVar t v
setCapturesInExpr (ExprConst t c) = pure $ ExprConst t c
setCapturesInExpr (ExprBinOp op lhs rhs) =
  ExprBinOp op <$> setCapturesInExpr lhs <*> setCapturesInExpr rhs
setCapturesInExpr (ExprUnOp op e) = ExprUnOp op <$> setCapturesInExpr e
setCapturesInExpr _ = undefined -- TODO: Remove when pattern synonyms have COMPLETE pragma.
