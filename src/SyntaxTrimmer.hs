module SyntaxTrimmer (trim) where

import Control.Monad
import Control.Monad.RWS (RWS, execRWS)
import qualified Control.Monad.RWS as RWS
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Syntax

trim :: Program -> Program
trim p = trimVariables ar $ trimFunctions ar p
  where
    ar = collectUsages p

collectUsages :: Program -> AccessRecorder
collectUsages p = usages `mappend` mainFunUsage
  where
    (_, usages) = execRWS (collectUsagesInFunction (FunID 0)) (programFunctions p) IntSet.empty
    mainFunUsage = mempty
      { funAccess = IntSet.singleton 0
      }

type CollectUsages = RWS (IntMap FunctionDef) AccessRecorder IntSet

collectUsagesInFunction :: FunID -> CollectUsages ()
collectUsagesInFunction (FunID fid) = do
  visited <- RWS.get
  when (IntSet.notMember fid visited) $ do
    RWS.modify $ IntSet.insert fid
    Just fdef <- IntMap.lookup fid <$> RWS.ask
    RWS.tell $ funDefAccesses fdef
    let gs = IntSet.toList $ funAccess $ funDefAccesses fdef
    forM_ gs $ \g -> do
      collectUsagesInFunction (FunID g)

trimFunctions :: AccessRecorder -> Program -> Program
trimFunctions ar p = p
  { programFunctions = keepKeys (funAccess ar) (programFunctions p)
  , programForeignFunctions = keepKeys (foreignFunAccess ar) (programForeignFunctions p)
  }
  where
    keepKeys ks = IntMap.filterWithKey (\k _ -> k `IntSet.member` ks)

trimVariables :: AccessRecorder -> Program -> Program
trimVariables ar p = p
  { programFunctions = trimVariablesInFunction ar <$> programFunctions p
  }

trimVariablesInFunction :: AccessRecorder -> FunctionDef -> FunctionDef
trimVariablesInFunction ar f = f
  { funDefBody = trimVariablesInBlock ar $ funDefBody f
  }

trimVariablesInBlock :: AccessRecorder -> Block -> Block
trimVariablesInBlock ar block = block
  { blockVariables = trimVariableList ar $ blockVariables block
  , blockStatements = trimVariablesInStatement ar <$> blockStatements block
  }

trimVariableList :: AccessRecorder -> [VarDecl] -> [VarDecl]
trimVariableList ar = filter go
  where
    go (VarDecl _ (VarID vid)) = IntSet.member vid (varAccess ar)

trimVariablesInStatement :: AccessRecorder -> Statement -> Statement
trimVariablesInStatement ar (StatementBlock b) = StatementBlock $ trimVariablesInBlock ar b
trimVariablesInStatement _ f@(StatementFunctionCall _) = f
trimVariablesInStatement ar (StatementWhile e b) = StatementWhile e $ trimVariablesInBlock ar b
trimVariablesInStatement _ f@(StatementAssign _ _) = f
trimVariablesInStatement ar (StatementIfElse e bt bf) = StatementIfElse e (trimVariablesInBlock ar bt) (trimVariablesInBlock ar bf)
trimVariablesInStatement _ f@(StatementReturn _) = f
