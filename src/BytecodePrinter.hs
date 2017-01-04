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
prettyPrintFunction f bf =
  "Function " ++ show f ++ ":\n" ++
  printVarsWithIndent 2 (bytecodeFunctionLocals bf) ++ "\n" ++
  printCodeWithIndent 2 (bytecodeFunctionOps bf)

printVarsWithIndent :: Int -> [VarDecl] -> String
printVarsWithIndent indent =
  concatMap (\(VarDecl t n) -> replicate indent ' ' ++ show t ++ " " ++ show n ++ "\n")

printCodeWithIndent :: Int -> [Op] -> String
printCodeWithIndent indent =
  concatMap (\op -> replicate indent ' ' ++ show op ++ "\n")
