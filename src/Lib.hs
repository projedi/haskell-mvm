module Lib
  ( someFunc
  ) where

import Control.Monad (forM_)
import System.Environment (getArgs)

import Eval (eval)
import Parser (parseExpr)

evaluateFile :: FilePath -> IO ()
evaluateFile fname = do
  contents <- readFile fname
  let expr = parseExpr contents
  eval expr

someFunc :: IO ()
someFunc = do
  args <- getArgs
  forM_ args $ \fname -> evaluateFile fname
