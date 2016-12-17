module BytecodePrinter
  ( prettyPrint
  ) where

import qualified Data.IntMap as IntMap

import Bytecode

prettyPrint :: Bytecode -> String
prettyPrint (Bytecode funs) =
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
