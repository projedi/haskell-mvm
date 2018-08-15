module Lib
  ( someFunc
  ) where

import Control.Monad (forM_)
import System.Environment (getArgs)

import ASM
import qualified ASMSyntax
import BinaryTranslator (withBinary)
import qualified EvalASM
import qualified EvalSimplified
import qualified JIT
import Parser (parseExpr)
import qualified PrettyPrintASM
import qualified PrettyPrintBinary
import qualified PrettyPrintSimplified
import qualified SimplifiedSyntax
import SyntaxLinearizer (linearize)
import SyntaxResolver (resolve)
import SyntaxSimplifier (simplify)
import TypeChecker (typeCheck)

getExpr :: String -> SimplifiedSyntax.Program
getExpr = simplify . typeCheck . resolve . parseExpr

getASM :: SimplifiedSyntax.Program -> ASMSyntax.Program
getASM = avenge . linearize

evaluateFileDumb :: FilePath -> IO ()
evaluateFileDumb fname = do
  contents <- readFile fname
  EvalSimplified.eval $ getExpr contents

evaluateFile :: FilePath -> IO ()
evaluateFile fname = do
  contents <- readFile fname
  EvalASM.eval $ getASM $ getExpr contents

jit :: FilePath -> IO ()
jit fname = do
  asm <- getASM . getExpr <$> readFile fname
  withBinary asm JIT.jit

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
  putStrLn $ PrettyPrintASM.prettyPrint asm

dumpASM :: FilePath -> IO ()
dumpASM fname = do
  asm <- (PrettyPrintASM.prettyPrint . getASM . getExpr) <$> readFile fname
  putStrLn asm

dumpBin :: FilePath -> IO ()
dumpBin fname = do
  asm <- getASM . getExpr <$> readFile fname
  binary <- withBinary asm PrettyPrintBinary.prettyPrint
  putStrLn binary

dumpJIT :: FilePath -> IO ()
dumpJIT fname = do
  asm <- getASM . getExpr <$> readFile fname
  withBinary asm $ \p -> do
    binary <- PrettyPrintBinary.prettyPrint p
    putStrLn binary
    JIT.jit p

getOperation :: [String] -> (FilePath -> IO (), [String])
getOperation [] = (const $ pure (), [])
getOperation ("--dumb":args) = (evaluateFileDumb, args)
getOperation ("--dump":args) = (dump, args)
getOperation ("--dump-asm":args) = (dumpASM, args)
getOperation ("--dump-bin":args) = (dumpBin, args)
getOperation ("--asm":args) = (evaluateFile, args)
getOperation ("--jit":args) = (jit, args)
getOperation ("--dump-jit":args) = (dumpJIT, args)
getOperation args = (jit, args)

someFunc :: IO ()
someFunc = do
  (operation, args) <- getOperation <$> getArgs
  forM_ args $ \fname -> operation fname
