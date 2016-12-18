module GoldenTests
  ( tests
  ) where

import Control.Monad (when)
import Data.Algorithm.Diff (Diff)
import qualified Data.Algorithm.Diff as Diff
import qualified Data.Algorithm.DiffOutput as DiffOutput
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import System.FilePath ((</>), (<.>), replaceExtension)
import qualified System.Process
import Test.Tasty
import Test.Tasty.Golden.Advanced

testList :: [String]
testList =
  [ "flops"
  , "for"
  , "fundef"
  , "if"
  , "intops"
  , "print"
  , "scoping"
  , "strops"
  , "vardef"
  , "while"
  , "fib"
  , "ffi"
  , "gnuplot/pm3d.8"
  ]

tests :: TestTree
tests =
  testGroup
    "Golden tests"
    [ testsWithParams "Dumb evaluate" 5 "--dumb"
    , testsWithParams "Bytecode interpret" 5 ""
    ]

testsWithParams :: String -> Integer -> String -> TestTree
testsWithParams name timeout flags =
  localOption (mkTimeout (1000000 * timeout)) $
  testGroup name $ map (test flags) testList

test :: String -> String -> TestTree
test flags name =
  goldenTest
    name
    (BS.unpack <$> BS.readFile goldenFile)
    (runEvaluateWithFlags flags (goldenFile `replaceExtension` "mvm"))
    compareResult
    (BS.writeFile goldenFile . BS.pack)
  where
    goldenFile = "examples" </> name <.> "expected"

runEvaluateWithFlags :: String -> FilePath -> IO String
runEvaluateWithFlags flags fname =
  System.Process.readCreateProcess
    (System.Process.shell $
     "stack exec mvm-haskell-exe -- " ++ flags ++ " " ++ fname)
    ""

compareResult :: String -> String -> IO (Maybe String)
compareResult lhs rhs = do
  let diff = Diff.getGroupedDiff (lines lhs) (lines rhs)
  if nonEmptyDiff diff
    then pure $ Just $ DiffOutput.ppDiff diff
    else pure Nothing

nonEmptyDiff :: [Diff a] -> Bool
nonEmptyDiff [] = False
nonEmptyDiff (Diff.Both _ _:diff) = nonEmptyDiff diff
nonEmptyDiff _ = True
