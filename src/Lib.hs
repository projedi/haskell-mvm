module Lib
  ( someFunc
  ) where

import Control.Monad (forM_)
import System.Environment (getArgs)

import qualified Eval
import qualified EvalSimplified
import qualified LinearSyntax
import Parser (parseExpr)
import qualified PrettyPrint
import qualified PrettyPrintSimplified
import qualified SimplifiedSyntax
import SyntaxLinearizer (linearize)
import SyntaxResolver (resolve)
import SyntaxSimplifier (simplify)
import TypeChecker (typeCheck)

getExpr :: String -> SimplifiedSyntax.Program
getExpr = simplify . typeCheck . resolve . parseExpr

getASM :: SimplifiedSyntax.Program -> LinearSyntax.Program
getASM = linearize

evaluateFileDumb :: FilePath -> IO ()
evaluateFileDumb fname = do
  contents <- readFile fname
  EvalSimplified.eval $ getExpr contents

evaluateFile :: FilePath -> IO ()
evaluateFile fname = do
  contents <- readFile fname
  Eval.eval $ getASM $ getExpr contents

dump :: FilePath -> IO ()
dump fname = do
  contents <- readFile fname
  putStrLn $ "\nDoing: " ++ fname
  putStrLn "======= FILE ======="
  putStrLn contents
  let expr = getExpr contents
  putStrLn "======= CODE ======="
  putStrLn $ PrettyPrintSimplified.prettyPrint expr
  putStrLn "======= ASM  ======="
  let asm = getASM expr
  putStrLn $ PrettyPrint.prettyPrint asm

getOperation :: [String] -> (FilePath -> IO (), [String])
getOperation [] = (const $ pure (), [])
getOperation ("--dumb":args) = (evaluateFileDumb, args)
getOperation ("--dump":args) = (dump, args)
getOperation ("--asm":args) = (evaluateFile, args)
getOperation args = (evaluateFile, args)

someFunc :: IO ()
someFunc = do
  (operation, args) <- getOperation <$> getArgs
  forM_ args $ \fname -> operation fname
