module CBench (test) where

import Criterion.Main
import System.Exit (ExitCode(..))
import System.FilePath ((</>), (<.>), replaceExtension)
import qualified System.Process

test :: String -> [String] -> String -> Benchmark
test name flags fname =
  bench name $ nfIO $ runEvaluateWithFlags flags ("examples" </> fname <.> "c")

runEvaluateWithFlags :: [String] -> FilePath -> IO String
runEvaluateWithFlags flags fname = do
  compile flags fname
  run

compile :: [String] -> FilePath -> IO ()
compile flags fname = do
  (ec, _, err) <- System.Process.readProcessWithExitCode "gcc" (flags ++ ["-o", "/tmp/a.out", "-O2", fname]) ""
  case (ec, err) of
    (ExitSuccess, []) -> pure ()
    (ExitSuccess, _) -> putStrLn err
    (_, []) -> error $ show ec
    (_, _) -> error $ show ec ++ ":\n" ++ err

run :: IO String
run = do
  (ec, res, err) <- System.Process.readProcessWithExitCode "/tmp/a.out" [] ""
  case (ec, err) of
    (ExitSuccess, []) -> pure res
    (ExitSuccess, _) -> putStrLn err >> pure res
    (_, []) -> error $ show ec
    (_, _) -> error $ show ec ++ ":\n" ++ err
