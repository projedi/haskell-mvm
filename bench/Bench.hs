import Criterion.Main

import qualified CBench
import qualified MVMBench

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
  , testWithParams "JIT" [] names
  ]
  where
  names = perfTestList ++ originalPerfTestList

testWithParams :: String -> [String] -> [String] -> Benchmark
testWithParams name flags names = bgroup name $ map (test flags) names

test :: [String] -> String -> Benchmark
test flags name = bgroup name
  [ MVMBench.test "MVM-pure" flags (name ++ "-pure")
  , MVMBench.test "MVM-ffi" flags (name ++ "-ffi")
  , CBench.test "C" ["-lm"] name
  ]
