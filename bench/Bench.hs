import Criterion.Main
import System.Exit (ExitCode(..))
import System.FilePath ((</>), (<.>), replaceExtension)
import qualified System.Process

main :: IO ()
main = defaultMain [ perfTests ]

perfTestList :: [String]
perfTestList = []

-- TODO: Convert them to benchmarkable things
originalPerfTestList :: [String]
originalPerfTestList =
  [ "original/perf/newton"
  , "original/perf/prime"
  ]

perfTests :: Benchmark
perfTests = bgroup "Perf"
  [ testWithParams "Dumb" ["--dumb"] names
  , testWithParams "Bytecode" ["--eval"] names
  , testWithParams "JIT" [] names
  ]
  where
  names = perfTestList ++ originalPerfTestList

testWithParams :: String -> [String] -> [String] -> Benchmark
testWithParams name flags names = bgroup name $ map (test flags) names

test :: [String] -> String -> Benchmark
test flags name =
  bench name $ nfIO $ runEvaluateWithFlags flags ("examples" </> name <.> "mvm")

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
