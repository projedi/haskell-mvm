module GoldenTests
  ( tests
  ) where

import Control.Monad (when)
import Data.Algorithm.Diff (Diff)
import qualified Data.Algorithm.Diff as Diff
import qualified Data.Algorithm.DiffOutput as DiffOutput
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import System.Exit (ExitCode(..))
import System.FilePath ((<.>), (</>), replaceExtension)
import qualified System.Process
import Test.Tasty
import Test.Tasty.ExpectedFailure (expectFail)
import Test.Tasty.Golden.Advanced

testList :: [String]
testList =
  [ "boolops"
  , "casts"
  , "flops"
  , "for"
  , "fundef"
  , "if"
  , "intops"
  , "print"
  , "scoping"
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
intensiveTestList = ["fib"]

originalIntensiveTestList :: [String]
originalIntensiveTestList =
  [ "original/additional/ackermann"
  , "original/additional/ackermann_closure"
  , "original/additional/complex2"
  , "original/additional/fail/vars" -- we don't consider it as failure
  , "original/additional/fib"
  , "original/additional/fib_closure"
  , "original/additional/vars"
  ]

failTestList :: [String]
failTestList = ["strops.fail"]

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
  ]

graphicsTestList :: [String]
graphicsTestList = ["graphics/pm3d.8"]

originalGraphicsTestList :: [String]
originalGraphicsTestList = ["original/optional/plot"]

{- These require custom SDL bindings. So disable them for now.
sdlTestList :: [String]
sdlTestList =
  [ "original/perf/graph_plot"
  , "original/perf/lissajous"
  , "original/perf/plot"
  ]
-}
tests :: TestTree
tests = testGroup "Golden" [passTests, failTests, intensiveTests, graphicsTests]

data TestRuntime
  = TestRuntimeDumb
  | TestRuntimeASM
  | TestRuntimeRealASM

runtimeName :: TestRuntime -> String
runtimeName TestRuntimeDumb = "Dumb"
runtimeName TestRuntimeASM = "ASM"
runtimeName TestRuntimeRealASM = "RealASM"

passTests :: TestTree
passTests =
  testGroup
    "Pass"
    [ testsWithParams TestRuntimeDumb 1 "expected" names
    , testsWithParams TestRuntimeASM 1 "expected" names
    , testsWithParams TestRuntimeRealASM 1 "expected" names
    ]
  where
    names = testList ++ originalTestList

failTests :: TestTree
failTests =
  expectFail $
  testGroup
    "Fail"
    [ testsWithParams TestRuntimeDumb 1 "expected" names
    , testsWithParams TestRuntimeASM 1 "expected" names
    , testsWithParams TestRuntimeRealASM 1 "expected" names
    ]
  where
    names = failTestList ++ originalFailTestList

intensiveTests :: TestTree
intensiveTests =
  testGroup
    "Intensive"
    [ testsWithParams TestRuntimeDumb 30 "expected" names
    , testsWithParams TestRuntimeASM 30 "expected" names
    , testsWithParams TestRuntimeRealASM 30 "expected" names
    ]
  where
    names = intensiveTestList ++ originalIntensiveTestList

graphicsTests :: TestTree
graphicsTests =
  testGroup
    "Graphics"
    [ testsWithParams TestRuntimeDumb 30 "ppm" names
    , testsWithParams TestRuntimeASM 30 "ppm" names
    , testsWithParams TestRuntimeRealASM 30 "expected" names
    ]
  where
    names = graphicsTestList ++ originalGraphicsTestList

testsWithParams :: TestRuntime -> Integer -> String -> [String] -> TestTree
testsWithParams runtime timeout expectedExtension names =
  localOption (mkTimeout (1000000 * timeout)) $
  testGroup (runtimeName runtime) $ map (test runtime expectedExtension) names

test :: TestRuntime -> String -> String -> TestTree
test runtime expectedExtension name =
  goldenTest
    name
    (BS.unpack <$> BS.readFile goldenFile)
    (runEvaluateWithFlags runtime (goldenFile `replaceExtension` "mvm"))
    compareResult
    (BS.writeFile goldenFile . BS.pack)
  where
    goldenFile = "examples" </> name <.> expectedExtension

runEvaluateWithFlags :: TestRuntime -> FilePath -> IO String
runEvaluateWithFlags runtime fname = do
  (ec, res, err) <-
    System.Process.readProcessWithExitCode
      "/bin/sh"
      (["-c", cmd runtime ++ " " ++ fname])
      ""
  case (ec, err) of
    (ExitSuccess, []) -> pure res
    (ExitSuccess, _) -> putStrLn err >> pure res
    (_, []) -> error $ show ec
    (_, _) -> error $ show ec ++ ":\n" ++ err
  where
    cmd TestRuntimeDumb = "mvm-haskell-exe --dumb"
    cmd TestRuntimeASM = "mvm-haskell-exe --asm"
    cmd TestRuntimeRealASM = "test/runasm.sh"

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
