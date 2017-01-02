module BytecodePrinter
  ( prettyPrint
  ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Bytecode
import Value (Value)

prettyPrint :: Bytecode -> String
prettyPrint bc =
  unlines
    [ prettyPrintLibs $ bytecodeLibraries bc
    , prettyPrintConstants $ bytecodeConstants bc
    , prettyPrintForeignFuns $ bytecodeForeignFunctions bc
    , prettyPrintFuns $ bytecodeFunctions bc
    ]

prettyPrintLibs :: [String] -> String
prettyPrintLibs libs = "Libraries: " ++ unwords libs

prettyPrintConstants :: IntMap Value -> String
prettyPrintConstants consts =
  "Constants: " ++
  IntMap.foldrWithKey
    (\key val rest -> rest ++ " " ++ show key ++ ":" ++ show val)
    ""
    consts

prettyPrintForeignFuns :: IntMap ForeignFunctionDecl -> String
prettyPrintForeignFuns funs =
  "Foreign functions: " ++
  IntMap.foldrWithKey
    (\key val rest ->
        rest ++ "\n" ++ show key ++ ": " ++ prettyPrintForeignFun val)
    ""
    funs

prettyPrintForeignFun :: ForeignFunctionDecl -> String
prettyPrintForeignFun fdecl =
  show (foreignFunDeclRetType fdecl) ++
  " " ++
  foreignFunDeclRealName fdecl ++ " " ++ show (foreignFunDeclParams fdecl)

prettyPrintFuns :: IntMap BytecodeFunction -> String
prettyPrintFuns =
  IntMap.foldrWithKey
    (\key code rest -> rest ++ "\n" ++ prettyPrintFunction key code)
    ""

prettyPrintFunction :: Int -> BytecodeFunction -> String
prettyPrintFunction f (BytecodeFunction code) =
  "Function " ++ show f ++ ":\n" ++ printCodeWithIndent 2 code

printCodeWithIndent :: Int -> [Op] -> String
printCodeWithIndent indent =
  concatMap (\op -> replicate indent ' ' ++ show op ++ "\n")
