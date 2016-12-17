import Test.Tasty

import qualified GoldenTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [GoldenTests.tests]
