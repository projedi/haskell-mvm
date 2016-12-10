module Lib
  ( someFunc
  ) where

import Control.Monad (forM_)
import System.Environment (getArgs)

import Parser (parseExpr)

someFunc :: IO ()
someFunc = do
  args <- getArgs
  forM_ args $ \fname -> do
    putStrLn $ "Parsing " ++ fname
    print =<< parseExpr <$> readFile fname
