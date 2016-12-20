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
import System.Exit (ExitCode(..))
import qualified System.Process
import Test.Tasty
import Test.Tasty.ExpectedFailure (expectFail)
import Test.Tasty.Golden.Advanced

testList :: [String]
testList =
  [ "boolops"
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
  , "ffi"
  ]

originalTestList :: [String]
originalTestList =
  [ "original/add"
  , "original/additional/casts"
  , "original/additional/closure"
  , "original/additional/complex"
  , "original/additional/function-call"
  , "original/additional/function-cast"
  , "original/additional/function"
  , "original/assign"
  , "original/bitwise"
  , "original/div"
  , "original/expr"
  , "original/for"
  , "original/function"
  , "original/if"
  , "original/literal"
  , "original/mul"
  , "original/optional/function_native"
  , "original/sub"
  , "original/while"
  ]

intensiveTestList :: [String]
intensiveTestList = ["fib", "gnuplot/pm3d.8"]

originalIntensiveTestList :: [String]
originalIntensiveTestList =
  [ "original/additional/ackermann"
  , "original/additional/ackermann_closure"
  , "original/additional/complex2"
  , "original/additional/fib"
  , "original/additional/fib_closure"
  , "original/additional/vars"
  ]

failTestList :: [String]
failTestList = []

originalFailTestList :: [String]
originalFailTestList =
  [ "original/additional/fail/for_range"
  , "original/additional/fail/for_var"
  , "original/additional/fail/function-cast"
  , "original/additional/fail/function-return-void"
  , "original/additional/fail/if-fun"
  , "original/additional/fail/op_bin"
  , "original/additional/fail/op_not"
  , "original/additional/fail/op_streq"
  , "original/additional/fail/op_sub"
  , "original/additional/fail/range"
  , "original/additional/fail/vars"
  ]

{- These require custom SDL bindings. So disable them for now.
sdlTestList :: [String]
sdlTestList =
  [ "original/optional/plot"
  , "original/perf/graph_plot"
  , "original/perf/lissajous"
  , "original/perf/plot"
  ]
-}

{- These are perf tests which are better be executed via stack bench.
originalPerfTestList :: [String]
originalPerfTestList =
  [ "original/perf/newton"
  , "original/perf/prime"
  ]
-}

tests :: TestTree
tests = testGroup "Golden" [passTests, failTests, intensiveTests]

passTests :: TestTree
passTests =
  testGroup
    "Pass"
    [ testsWithParams "Dumb" 1 ["--dumb"] names
    , testsWithParams "Bytecode" 1 ["--eval"] names
    , testsWithParams "JIT" 1 [] names
    ]
  where
    names = testList ++ originalTestList

failTests :: TestTree
failTests =
  expectFail $
  testGroup
    "Fail"
    [ testsWithParams "Dumb" 30 ["--dumb"] names
    , testsWithParams "Bytecode" 30 ["--eval"] names
    , testsWithParams "JIT" 30 [] names
    ]
  where
    names = failTestList ++ originalFailTestList

intensiveTests :: TestTree
intensiveTests =
  testGroup
    "Intensive"
    [ testsWithParams "Dumb" 300 ["--dumb"] names
    , testsWithParams "Bytecode" 300 ["--eval"] names
    , testsWithParams "JIT" 300 [] names
    ]
  where
    names = intensiveTestList ++ originalIntensiveTestList

testsWithParams :: String -> Integer -> [String] -> [String] -> TestTree
testsWithParams name timeout flags names =
  localOption (mkTimeout (1000000 * timeout)) $
  testGroup name $ map (test flags) names

test :: [String] -> String -> TestTree
test flags name =
  goldenTest
    name
    (BS.unpack <$> BS.readFile goldenFile)
    (runEvaluateWithFlags flags (goldenFile `replaceExtension` "mvm"))
    compareResult
    (BS.writeFile goldenFile . BS.pack)
  where
    goldenFile = "examples" </> name <.> "expected"

runEvaluateWithFlags :: [String] -> FilePath -> IO String
runEvaluateWithFlags flags fname = do
  (ec, res, err) <-
    System.Process.readProcessWithExitCode
      "mvm-haskell-exe"
      (flags ++ [fname])
      ""
  case (ec, err) of
    (ExitSuccess, []) -> pure res
    (ExitSuccess, _) -> putStrLn err >> pure res
    (_, []) -> error $ show ec
    (_, _) -> error $ show ec ++ ":\n" ++ err

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
