module GoldenTests (tests) where

import Control.Monad (when)
import Data.Algorithm.Diff (Diff)
import qualified Data.Algorithm.Diff as Diff
import qualified Data.Algorithm.DiffOutput as DiffOutput
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import System.FilePath ((</>), (<.>), replaceExtension)
import System.IO.Silently (capture_)
import Test.Tasty
import Test.Tasty.Golden.Advanced

import Lib (evaluateFile)

testList :: [String]
testList =
  [ "fib"
  , "flops"
  , "for"
  , "fundef"
  , "if"
  , "intops"
  , "print"
  , "scoping"
  , "strops"
  , "vardef"
  , "while"
  , "gnuplot/pm3d.8"
  ]

tests :: TestTree
tests = localOption (mkTimeout 1000000) $ testGroup "Golden tests" $ map test testList

test :: String -> TestTree
test name =
  goldenTest
    name
    (BS.unpack <$> BS.readFile goldenFile)
    (runEvaluate (goldenFile `replaceExtension` "mvm"))
    compareResult
    (BS.writeFile goldenFile . BS.pack)
 where
  goldenFile = "examples" </> name <.> "expected"

runEvaluate :: FilePath -> IO String
runEvaluate fname =
  capture_ $ evaluateFile fname

compareResult :: String -> String -> IO (Maybe String)
compareResult lhs rhs = do
  let diff = Diff.getGroupedDiff (lines lhs) (lines rhs)
  if (nonEmptyDiff diff) then pure $ Just $ DiffOutput.ppDiff diff else pure Nothing

nonEmptyDiff :: [Diff a] -> Bool
nonEmptyDiff [] = False
nonEmptyDiff (Diff.Both _ _ : diff) = nonEmptyDiff diff
nonEmptyDiff _ = True