module VarUsageResolver
  ( UsageEntry(..)
  , resolveVarUsage
  ) where

import Control.Monad
import Control.Monad.State (State, execState)
import qualified Control.Monad.State as State
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

data UsageEntry = UsageEntry
  { vars :: [Int]
  , locals :: [Int]
  , funs :: [Int]
  }

data ResolverState = ResolverState
  { graph :: IntMap [Int]
  , localUsages :: IntMap IntSet
  , usages :: IntMap IntSet
  , visited :: IntSet
  }

resolveVarUsage :: IntMap UsageEntry -> IntMap [Int]
resolveVarUsage input =
  IntMap.map IntSet.toList $
  usages $
  snd
    (iterate
       go
       ( 0
       , ResolverState
           { graph = IntMap.map funs input
           , localUsages = IntMap.map (IntSet.fromList . locals) input
           , usages = IntMap.map (IntSet.fromList . vars) input
           , visited = IntSet.empty
           }) !!
     IntMap.size input)
  where
    go :: (Int, ResolverState) -> (Int, ResolverState)
    go (n, st) = (n + 1, execState (runResolverForFunction n) st)

runResolverForFunction :: Int -> State ResolverState ()
runResolverForFunction f = do
  alreadyVisited <- State.gets (IntSet.member f . visited)
  unless alreadyVisited $ do
    State.modify $ \st -> st {visited = IntSet.insert f $ visited st}
    nodes <- State.gets ((IntMap.! f) . graph)
    forM_ nodes $ \g -> do
      runResolverForFunction g
      gUsages <- externalUsages g
      State.modify $ \st ->
        st {usages = IntMap.adjust (IntSet.union gUsages) f $ usages st}

externalUsages :: Int -> State ResolverState IntSet
externalUsages f = do
  vs <- State.gets ((IntMap.! f) . usages)
  localVs <- State.gets ((IntMap.! f) . localUsages)
  pure $ vs IntSet.\\ localVs
