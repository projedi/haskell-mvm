module BytecodePrinter
  ( prettyPrint
  ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Bytecode

prettyPrint :: Bytecode -> String
prettyPrint bc =
  unlines
    [ prettyPrintLibs $ bytecodeLibraries bc
    , prettyPrintConstants $ bytecodeConstants bc
    , prettyPrintFuns $ bytecodeFunctions bc
    ]

prettyPrintLibs :: [String] -> String
prettyPrintLibs libs = "Libraries: " ++ unwords libs

prettyPrintConstants :: IntMap String -> String
prettyPrintConstants consts =
  "Constants: " ++
  IntMap.foldrWithKey
    (\key val rest -> rest ++ " " ++ show key ++ ":" ++ show val)
    ""
    consts

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
