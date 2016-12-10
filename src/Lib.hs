module Lib
  ( someFunc
  ) where

import Control.Monad (forM_)
import System.Environment (getArgs)

import Parser (parseExpr)
import PrettyPrint (prettyPrint)

someFunc :: IO ()
someFunc = do
  args <- getArgs
  forM_ args $ \fname -> do
    putStrLn $ "=== Parsing " ++ fname ++ " ==="
    contents <- readFile fname
    putStrLn $ contents
    let expr = parseExpr contents
    putStrLn $ "=== Result ==="
    putStrLn $ prettyPrint expr
