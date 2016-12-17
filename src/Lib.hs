module Lib
  ( someFunc
  ) where

import Control.Monad (forM_)
import System.Environment (getArgs)

import BytecodePrinter (prettyPrint)
import BytecodeTranslator (translate)
import Eval (eval)
import Parser (parseExpr)

evaluateFile :: FilePath -> IO ()
evaluateFile fname = do
  contents <- readFile fname
  let expr = parseExpr contents
  eval expr

printBytecode :: FilePath -> IO ()
printBytecode fname = do
  contents <- readFile fname
  putStrLn $ "\nDoing: " ++ fname
  putStrLn "======= CODE ======="
  putStrLn contents
  let expr = parseExpr contents
  putStrLn "===== BYTECODE ====="
  putStrLn $ prettyPrint $ translate expr

getOperation :: [String] -> (FilePath -> IO (), [String])
getOperation ("--dumb":args) = (evaluateFile, args)
getOperation args = (printBytecode, args)

someFunc :: IO ()
someFunc = do
  (operation, args) <- getOperation <$> getArgs
  forM_ args $ \fname -> operation fname
