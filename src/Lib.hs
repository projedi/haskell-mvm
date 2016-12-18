module Lib
  ( someFunc
  ) where

import Control.Monad (forM_)
import System.Environment (getArgs)

import BytecodePrinter (prettyPrint)
import qualified BytecodeInterpreter as Bytecode
import BytecodeTranslator (translate)
import Eval (eval)
import qualified JIT
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
  Bytecode.interpret bc

jit :: FilePath -> IO ()
jit fname = do
  contents <- readFile fname
  let expr = parseExpr contents
  let bc = translate expr
  JIT.interpret bc

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
getOperation ("--eval":args) = (interpretBytecode, args)
getOperation args = (jit, args)

someFunc :: IO ()
someFunc = do
  (operation, args) <- getOperation <$> getArgs
  forM_ args $ \fname -> operation fname
