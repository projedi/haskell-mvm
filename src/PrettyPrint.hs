{-# LANGUAGE FlexibleInstances #-}

module PrettyPrint
  ( prettyPrint
  ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import ASMSyntax

prettyPrint :: Program -> String
prettyPrint p =
  unlines
    [ printLibs $ programLibraries p
    , printStrings $ programStrings p
    , printCode $ programCode p
    ]

printLibs :: [String] -> String
printLibs libs = "# LINKER: " ++ unwords libs

printCode :: [Instruction] -> String
printCode body =
  unlines $ [".text", ".global _main", "_main: "] ++ map prettyPrintSimple body

printStrings :: IntMap String -> String
printStrings vals =
  ".data" ++
  IntMap.foldrWithKey
    (\key val rest ->
       rest ++ "\n" ++ show (StringID key) ++ ": .asciz " ++ show val)
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
  prettyPrintSimple ForeignFunctionCall {foreignFunCallRealName = funname} =
    funname

instance PrettyPrintSimple IntOperand where
  prettyPrintSimple (IntOperandRegister _ r) = prettyPrintSimple r
  prettyPrintSimple (IntOperandPointer p) = prettyPrintSimple p

instance PrettyPrintSimple Register where
  prettyPrintSimple RegisterRSP = "rsp"
  prettyPrintSimple RegisterRBP = "rbp"
  prettyPrintSimple RegisterRAX = "rax"
  prettyPrintSimple RegisterRBX = "rbx"
  prettyPrintSimple RegisterRDI = "rdi"
  prettyPrintSimple RegisterRSI = "rsi"
  prettyPrintSimple RegisterRDX = "rdx"
  prettyPrintSimple RegisterRCX = "rcx"
  prettyPrintSimple RegisterR8 = "r8"
  prettyPrintSimple RegisterR9 = "r9"

instance PrettyPrintSimple RegisterXMM where
  prettyPrintSimple RegisterXMM0 = "xmm0"
  prettyPrintSimple RegisterXMM1 = "xmm1"
  prettyPrintSimple RegisterXMM2 = "xmm2"
  prettyPrintSimple RegisterXMM3 = "xmm3"
  prettyPrintSimple RegisterXMM4 = "xmm4"
  prettyPrintSimple RegisterXMM5 = "xmm5"
  prettyPrintSimple RegisterXMM6 = "xmm6"
  prettyPrintSimple RegisterXMM7 = "xmm7"

instance PrettyPrintSimple Pointer where
  prettyPrintSimple Pointer {pointerBase = r, pointerDisplacement = d} =
    "[" ++ prettyPrintSimple r ++ "+" ++ show d ++ "]"

instance PrettyPrintSimple Instruction where
  prettyPrintSimple (InstructionCALL fcall) = "call " ++ prettyPrintSimple fcall
  prettyPrintSimple (InstructionCMP lhs rhs) =
    "cmp " ++ prettyPrintSimple lhs ++ ", " ++ prettyPrintSimple rhs
  prettyPrintSimple (InstructionSetZ v) = "setz " ++ prettyPrintSimple v
  prettyPrintSimple (InstructionSetNZ v) = "setnz " ++ prettyPrintSimple v
  prettyPrintSimple (InstructionSetS v) = "sets " ++ prettyPrintSimple v
  prettyPrintSimple (InstructionSetC v) = "setc " ++ prettyPrintSimple v
  prettyPrintSimple (InstructionMOV lhs rhs) =
    "mov " ++
    prettyPrintSimple lhs ++
    ", " ++ either prettyPrintSimple prettyPrintSimple rhs
  prettyPrintSimple InstructionRET = "ret"
  prettyPrintSimple (InstructionLabelledNOP l) = show l ++ ": nop"
  prettyPrintSimple (InstructionJMP l) = "jmp " ++ show l
  prettyPrintSimple (InstructionJZ l) = "jz " ++ show l
  prettyPrintSimple (InstructionNEG v) = "neg " ++ prettyPrintSimple v
  prettyPrintSimple (InstructionAND lhs rhs) =
    "and " ++ prettyPrintSimple lhs ++ ", " ++ prettyPrintSimple rhs
  prettyPrintSimple (InstructionXOR lhs rhs) =
    "xor " ++ prettyPrintSimple lhs ++ ", " ++ prettyPrintSimple rhs
  prettyPrintSimple (InstructionOR lhs rhs) =
    "or " ++ prettyPrintSimple lhs ++ ", " ++ prettyPrintSimple rhs
  prettyPrintSimple (InstructionADD lhs rhs) =
    "add " ++ prettyPrintSimple lhs ++ ", " ++ prettyPrintSimple rhs
  prettyPrintSimple (InstructionSUB lhs rhs) =
    "sub " ++ prettyPrintSimple lhs ++ ", " ++ prettyPrintSimple rhs
  prettyPrintSimple (InstructionIDIV v) = "idiv " ++ prettyPrintSimple v
  prettyPrintSimple (InstructionIMUL lhs rhs) =
    "imul " ++ prettyPrintSimple lhs ++ ", " ++ prettyPrintSimple rhs
  prettyPrintSimple InstructionCQO = "cqo"
  prettyPrintSimple (InstructionADDSD lhs rhs) =
    "addsd " ++ prettyPrintSimple lhs ++ ", " ++ prettyPrintSimple rhs
  prettyPrintSimple (InstructionSUBSD lhs rhs) =
    "subsd " ++ prettyPrintSimple lhs ++ ", " ++ prettyPrintSimple rhs
  prettyPrintSimple (InstructionMULSD lhs rhs) =
    "mulsd " ++ prettyPrintSimple lhs ++ ", " ++ prettyPrintSimple rhs
  prettyPrintSimple (InstructionDIVSD lhs rhs) =
    "divsd " ++ prettyPrintSimple lhs ++ ", " ++ prettyPrintSimple rhs
  prettyPrintSimple (InstructionCOMISD lhs rhs) =
    "comisd " ++ prettyPrintSimple lhs ++ ", " ++ prettyPrintSimple rhs
  prettyPrintSimple (InstructionMOVSD_XMM_XMM lhs rhs) =
    "movsd " ++ prettyPrintSimple lhs ++ ", " ++ prettyPrintSimple rhs
  prettyPrintSimple (InstructionMOVSD_XMM_M64 lhs rhs) =
    "movsd " ++ prettyPrintSimple lhs ++ ", " ++ prettyPrintSimple rhs
  prettyPrintSimple (InstructionMOVSD_M64_XMM lhs rhs) =
    "movsd " ++ prettyPrintSimple lhs ++ ", " ++ prettyPrintSimple rhs
  prettyPrintSimple (InstructionCVTSI2SD lhs rhs) =
    "cvtsi2sd " ++ prettyPrintSimple lhs ++ ", " ++ prettyPrintSimple rhs
  prettyPrintSimple (InstructionPUSH v) = "push " ++ prettyPrintSimple v
  prettyPrintSimple (InstructionPOP v) = "pop " ++ prettyPrintSimple v
  prettyPrintSimple (InstructionLEA r s) =
    "lea " ++ prettyPrintSimple r ++ ", [rip + " ++ show s ++ "]"

instance PrettyPrintSimple Immediate where
  prettyPrintSimple (ImmediateInt i) = show i
  prettyPrintSimple (ImmediateFloat f) = show f
