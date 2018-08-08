{-# LANGUAGE FlexibleInstances #-}

module PrettyPrint
  ( prettyPrint
  ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.List as List

import ASMSyntax

prettyPrint :: Program -> String
prettyPrint p =
  unlines
    [ printLibs $ programLibraries p
    , printForeignFunctions $ programForeignFunctions p
    , printStrings $ programStrings p
    , prettyPrintSimple $ programCode p
    ]

printLibs :: [String] -> String
printLibs libs = "Libraries: " ++ unwords libs

printForeignFunctions :: IntMap ForeignFunctionDecl -> String
printForeignFunctions funs =
  "Foreign functions: " ++
  IntMap.foldrWithKey
    (\key val rest -> rest ++ "\n" ++ show key ++ ": " ++ printForeignFun val)
    ""
    funs

printForeignFun :: ForeignFunctionDecl -> String
printForeignFun fdecl =
  show (foreignFunDeclRetType fdecl) ++
  " " ++
  foreignFunDeclRealName fdecl ++
  " " ++
  show (foreignFunDeclParams fdecl) ++
  (if foreignFunDeclHasVarArgs fdecl
     then " + varargs"
     else "")

printStrings :: IntMap String -> String
printStrings vals =
  "Strings: " ++
  IntMap.foldrWithKey
    (\key val rest -> rest ++ "\n" ++ show key ++ ": " ++ show val)
    ""
    vals

class PrettyPrintSimple a where
  prettyPrintSimple :: a -> String

instance PrettyPrintSimple FunID where
  prettyPrintSimple = show

instance PrettyPrintSimple LabelID where
  prettyPrintSimple = show

instance PrettyPrintSimple FunctionCall where
  prettyPrintSimple NativeFunctionCall {nativeFunCallName = funname} =
    prettyPrintSimple funname
  prettyPrintSimple ForeignFunctionCall {foreignFunCallName = funname} =
    prettyPrintSimple funname

instance PrettyPrintSimple VarType where
  prettyPrintSimple = show

instance PrettyPrintSimple (Maybe VarType) where
  prettyPrintSimple (Just vtype) = prettyPrintSimple vtype
  prettyPrintSimple Nothing = "void"

instance PrettyPrintSimple BinOp where
  prettyPrintSimple BinPlusFloat = "+"
  prettyPrintSimple BinMinusFloat = "-"
  prettyPrintSimple BinTimesFloat = "*"
  prettyPrintSimple BinDivFloat = "/"

instance PrettyPrintSimple IntOperand where
  prettyPrintSimple (IntOperandRegister _ r) = prettyPrintSimple r
  prettyPrintSimple (IntOperandPointer p) = prettyPrintSimple p

instance PrettyPrintSimple FloatOperand where
  prettyPrintSimple (FloatOperandRegister r) = prettyPrintSimple r
  prettyPrintSimple (FloatOperandPointer p) = prettyPrintSimple p

instance PrettyPrintSimple Register where
  prettyPrintSimple RegisterRSP = "RSP"
  prettyPrintSimple RegisterRBP = "RBP"
  prettyPrintSimple RegisterRAX = "RAX"
  prettyPrintSimple RegisterRDI = "RDI"
  prettyPrintSimple RegisterRSI = "RSI"
  prettyPrintSimple RegisterRDX = "RDX"
  prettyPrintSimple RegisterRCX = "RCX"
  prettyPrintSimple RegisterR8 = "R8"
  prettyPrintSimple RegisterR9 = "R9"

instance PrettyPrintSimple RegisterXMM where
  prettyPrintSimple RegisterXMM0 = "XMM0"
  prettyPrintSimple RegisterXMM1 = "XMM1"
  prettyPrintSimple RegisterXMM2 = "XMM2"
  prettyPrintSimple RegisterXMM3 = "XMM3"
  prettyPrintSimple RegisterXMM4 = "XMM4"
  prettyPrintSimple RegisterXMM5 = "XMM5"
  prettyPrintSimple RegisterXMM6 = "XMM6"
  prettyPrintSimple RegisterXMM7 = "XMM7"

instance PrettyPrintSimple Pointer where
  prettyPrintSimple Pointer {pointerBase = mr, pointerDisplacement = d} =
    "[" ++ (maybe "" ((++ "+") . prettyPrintSimple) mr) ++ show d ++ "]"

instance PrettyPrintSimple Statement where
  prettyPrintSimple (InstructionCALL fcall) = "CALL " ++ prettyPrintSimple fcall
  prettyPrintSimple (StatementBinOp op el er) =
    prettyPrintSimple el ++
    " " ++ prettyPrintSimple op ++ " " ++ prettyPrintSimple er ++ ";"
  prettyPrintSimple (InstructionCMP lhs rhs) =
    "CMP " ++ prettyPrintSimple lhs ++ " " ++ prettyPrintSimple rhs
  prettyPrintSimple (InstructionSetZ v) = "SETZ " ++ prettyPrintSimple v
  prettyPrintSimple (InstructionSetNZ v) = "SETNZ " ++ prettyPrintSimple v
  prettyPrintSimple (InstructionSetS v) = "SETS " ++ prettyPrintSimple v
  prettyPrintSimple (InstructionSetC v) = "SETC " ++ prettyPrintSimple v
  prettyPrintSimple (InstructionMOV lhs rhs) =
    "MOV " ++
    prettyPrintSimple lhs ++
    " " ++ either prettyPrintSimple prettyPrintSimple rhs
  prettyPrintSimple (StatementPushOnStack x) =
    "push " ++ prettyPrintSimple x ++ ";"
  prettyPrintSimple (StatementAllocateOnStack t) =
    "alloc " ++ prettyPrintSimple t ++ ";"
  prettyPrintSimple (StatementPopFromStack t) =
    "pop " ++ prettyPrintSimple t ++ ";"
  prettyPrintSimple InstructionRET = "RET"
  prettyPrintSimple (InstructionLabelledNOP l) = show l ++ ": NOP"
  prettyPrintSimple (InstructionJMP l) = "JMP " ++ show l
  prettyPrintSimple (InstructionJZ l) = "JZ " ++ show l
  prettyPrintSimple (InstructionNEG v) = "NEG " ++ prettyPrintSimple v
  prettyPrintSimple (InstructionAND lhs rhs) =
    "AND " ++ prettyPrintSimple lhs ++ " " ++ prettyPrintSimple rhs
  prettyPrintSimple (InstructionXOR lhs rhs) =
    "XOR " ++ prettyPrintSimple lhs ++ " " ++ prettyPrintSimple rhs
  prettyPrintSimple (InstructionOR lhs rhs) =
    "OR " ++ prettyPrintSimple lhs ++ " " ++ prettyPrintSimple rhs
  prettyPrintSimple (InstructionADD lhs rhs) =
    "ADD " ++ prettyPrintSimple lhs ++ " " ++ prettyPrintSimple rhs
  prettyPrintSimple (InstructionSUB lhs rhs) =
    "SUB " ++ prettyPrintSimple lhs ++ " " ++ prettyPrintSimple rhs
  prettyPrintSimple (InstructionIDIV v) = "IDIV " ++ prettyPrintSimple v
  prettyPrintSimple (InstructionIMUL lhs rhs) =
    "IMUL " ++ prettyPrintSimple lhs ++ " " ++ prettyPrintSimple rhs
  prettyPrintSimple InstructionCQO = "CQO"
  prettyPrintSimple (InstructionADDSD lhs rhs) =
    "ADDSD " ++ prettyPrintSimple lhs ++ " " ++ prettyPrintSimple rhs
  prettyPrintSimple (InstructionSUBSD lhs rhs) =
    "SUBSD " ++ prettyPrintSimple lhs ++ " " ++ prettyPrintSimple rhs
  prettyPrintSimple (InstructionMULSD lhs rhs) =
    "MULSD " ++ prettyPrintSimple lhs ++ " " ++ prettyPrintSimple rhs
  prettyPrintSimple (InstructionDIVSD lhs rhs) =
    "DIVSD " ++ prettyPrintSimple lhs ++ " " ++ prettyPrintSimple rhs
  prettyPrintSimple (InstructionCOMISD lhs rhs) =
    "COMISD " ++ prettyPrintSimple lhs ++ " " ++ prettyPrintSimple rhs
  prettyPrintSimple (InstructionMOVSD_XMM_XMM lhs rhs) =
    "MOVSD " ++ prettyPrintSimple lhs ++ " " ++ prettyPrintSimple rhs
  prettyPrintSimple (InstructionMOVSD_XMM_M64 lhs rhs) =
    "MOVSD " ++ prettyPrintSimple lhs ++ " " ++ prettyPrintSimple rhs
  prettyPrintSimple (InstructionMOVSD_M64_XMM lhs rhs) =
    "MOVSD " ++ prettyPrintSimple lhs ++ " " ++ prettyPrintSimple rhs
  prettyPrintSimple (InstructionCVTSI2SD lhs rhs) =
    "CVTSI2SD " ++ prettyPrintSimple lhs ++ " " ++ prettyPrintSimple rhs

instance PrettyPrintSimple FunctionDef where
  prettyPrintSimple FunctionDef {funDefBody = body} =
    "Code:\n" ++ List.intercalate "\n" (map printLine (zip [0 ..] body))
    where
      printLine :: (Int, Statement) -> String
      printLine (line, stmt) = show line ++ ": " ++ prettyPrintSimple stmt

instance PrettyPrintSimple Immediate where
  prettyPrintSimple (ImmediateInt i) = show i
  prettyPrintSimple (ImmediateFloat f) = show f
  prettyPrintSimple (ImmediateString s) = show s
