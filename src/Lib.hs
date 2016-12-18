module Lib
  ( someFunc
  ) where

import Control.Monad (forM_)
import System.Environment (getArgs)

import BytecodePrinter (prettyPrint)
import BytecodeInterpreter (interpret)
import BytecodeTranslator (translate)
import Eval (eval)
import Parser (parseExpr)

evaluateFile :: FilePath -> IO ()
evaluateFile fname = do
  contents <- readFile fname
  let expr = parseExpr contents
  eval expr

interpretBytecode :: FilePath -> IO ()
interpretBytecode fname = do
  contents <- readFile fname
  let expr = parseExpr contents
  let bc = translate expr
  interpret bc

dumpBytecode :: FilePath -> IO ()
dumpBytecode fname = do
  contents <- readFile fname
  putStrLn $ "\nDoing: " ++ fname
  putStrLn "======= CODE ======="
  putStrLn contents
  let expr = parseExpr contents
  putStrLn "===== BYTECODE ====="
  putStrLn $ prettyPrint $ translate expr

getOperation :: [String] -> (FilePath -> IO (), [String])
getOperation ("--dumb":args) = (evaluateFile, args)
getOperation ("--dump":args) = (dumpBytecode, args)
getOperation args = (interpretBytecode, args)

someFunc :: IO ()
someFunc = do
  (operation, args) <- getOperation <$> getArgs
  forM_ args $ \fname -> operation fname
