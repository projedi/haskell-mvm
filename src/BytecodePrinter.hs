module BytecodePrinter
  ( prettyPrint
  ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Bytecode

prettyPrint :: Bytecode -> String
prettyPrint (Bytecode funs libs) = prettyPrintLibs libs ++ prettyPrintFuns funs

prettyPrintLibs :: [String] -> String
prettyPrintLibs libs = "Libraries: " ++ unwords libs

prettyPrintFuns :: IntMap BytecodeFunction -> String
prettyPrintFuns funs =
  IntMap.foldrWithKey
    (\key code rest -> rest ++ "\n" ++ prettyPrintFunction key code)
    ""
    funs

prettyPrintFunction :: Int -> BytecodeFunction -> String
prettyPrintFunction f (BytecodeFunction code) =
  "Function " ++ show f ++ ":\n" ++ printCodeWithIndent 2 code

printCodeWithIndent :: Int -> [Op] -> String
printCodeWithIndent indent =
  concatMap (\op -> replicate indent ' ' ++ show op ++ "\n")
