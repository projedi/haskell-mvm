module MVMBench (test) where

import Criterion.Main
import System.Exit (ExitCode(..))
import System.FilePath ((</>), (<.>), replaceExtension)
import qualified System.Process

test :: String -> [String] -> String -> Benchmark
test name flags fname =
  bench name $ nfIO $ runEvaluateWithFlags flags ("examples" </> fname <.> "mvm")

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
