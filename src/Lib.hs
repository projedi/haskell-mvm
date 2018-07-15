module Lib
  ( someFunc
  ) where

import Control.Monad (forM_)
import System.Environment (getArgs)

import Eval (eval)
import qualified JIT
import Parser (parseExpr)
import PrettyPrint (prettyPrint)
import SyntaxResolver (resolve)
import TypeChecker (typeCheck)
import TypedSyntax (Program)

getExpr :: String -> Program
getExpr = typeCheck . resolve . parseExpr

evaluateFile :: FilePath -> IO ()
evaluateFile fname = do
  contents <- readFile fname
  let expr = getExpr contents
  eval expr

jit :: FilePath -> IO ()
jit fname = do
  contents <- readFile fname
  let expr = getExpr contents
  JIT.interpret expr

dump :: FilePath -> IO ()
dump fname = do
  contents <- readFile fname
  putStrLn $ "\nDoing: " ++ fname
  putStrLn "======= FILE ======="
  putStrLn contents
  let expr = getExpr contents
  putStrLn "======= CODE ======="
  putStrLn $ prettyPrint expr

getOperation :: [String] -> (FilePath -> IO (), [String])
getOperation [] = (const $ pure (), [])
getOperation ("--dumb":args) = (evaluateFile, args)
getOperation ("--dump":args) = (dump, args)
getOperation args = (jit, args)

someFunc :: IO ()
someFunc = do
  (operation, args) <- getOperation <$> getArgs
  forM_ args $ \fname -> operation fname
