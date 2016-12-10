module Lib
  ( someFunc
  ) where

import Control.Monad (forM_)
import qualified System.FilePath as FilePath
import System.Environment (getArgs)

import Eval (eval)
import Parser (parseExpr)

someFunc :: IO ()
someFunc = do
  args <- getArgs
  forM_ args $ \fname -> do
    putStrLn $ "=== Evaluating " ++ fname ++ " ==="
    putStrLn $ "Expected:"
    expected <- readFile $ FilePath.replaceExtension fname "expected"
    putStrLn $ expected
    contents <- readFile fname
    let expr = parseExpr contents
    putStrLn $ "Actual:"
    eval expr
