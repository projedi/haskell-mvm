{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module PrettyPrint
  ( prettyPrint
  ) where

import Data.Array.ST (MArray, STUArray, newArray, readArray)
import Data.Array.Unsafe (castSTUArray)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Word (Word64)
import GHC.ST (ST, runST)

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

instance PrettyPrintSimple Register8 where
  prettyPrintSimple RegisterAL = "al"
  prettyPrintSimple RegisterCL = "cl"
  prettyPrintSimple RegisterDL = "dl"

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
    "qword ptr [" ++ prettyPrintSimple r ++ printDisplacement d ++ "]"
    where
      printDisplacement 0 = ""
      printDisplacement x
        | x < 0 = " - " ++ show (-x)
        | otherwise = " + " ++ show x

printUnOp :: (PrettyPrintSimple t) => String -> t -> String
printUnOp n t = n ++ " " ++ prettyPrintSimple t

printBinOp ::
     (PrettyPrintSimple t1, PrettyPrintSimple t2)
  => String
  -> t1
  -> t2
  -> String
printBinOp n t1 t2 =
  n ++ " " ++ prettyPrintSimple t1 ++ ", " ++ prettyPrintSimple t2

instance PrettyPrintSimple Instruction where
  prettyPrintSimple (InstructionCALL fcall) = printUnOp "call" fcall
  prettyPrintSimple (InstructionCMP lhs rhs) = printBinOp "cmp" lhs rhs
  prettyPrintSimple (InstructionSetZ v) = printUnOp "setz" v
  prettyPrintSimple (InstructionSetNZ v) = printUnOp "setnz" v
  prettyPrintSimple (InstructionSetS v) = printUnOp "sets" v
  prettyPrintSimple (InstructionSetC v) = printUnOp "setc" v
  prettyPrintSimple (InstructionMOV_R64_IMM64 lhs rhs) =
    printBinOp "mov" lhs rhs
  prettyPrintSimple (InstructionMOV_R64_RM64 lhs rhs) = printBinOp "mov" lhs rhs
  prettyPrintSimple (InstructionMOV_RM64_R64 lhs rhs) = printBinOp "mov" lhs rhs
  prettyPrintSimple InstructionRET = "ret"
  prettyPrintSimple (InstructionLabelledNOP l) = show l ++ ": nop"
  prettyPrintSimple (InstructionJMP l) = "jmp " ++ show l
  prettyPrintSimple (InstructionJZ l) = "jz " ++ show l
  prettyPrintSimple (InstructionNEG v) = printUnOp "neg" v
  prettyPrintSimple (InstructionAND lhs rhs) = printBinOp "and" lhs rhs
  prettyPrintSimple (InstructionXOR lhs rhs) = printBinOp "xor" lhs rhs
  prettyPrintSimple (InstructionOR lhs rhs) = printBinOp "or" lhs rhs
  prettyPrintSimple (InstructionADD lhs rhs) = printBinOp "add" lhs rhs
  prettyPrintSimple (InstructionSUB lhs rhs) = printBinOp "sub" lhs rhs
  prettyPrintSimple (InstructionIDIV v) = printUnOp "idiv" v
  prettyPrintSimple (InstructionIMUL lhs rhs) = printBinOp "imul" lhs rhs
  prettyPrintSimple InstructionCQO = "cqo"
  prettyPrintSimple (InstructionADDSD lhs rhs) = printBinOp "addsd" lhs rhs
  prettyPrintSimple (InstructionSUBSD lhs rhs) = printBinOp "subsd" lhs rhs
  prettyPrintSimple (InstructionMULSD lhs rhs) = printBinOp "mulsd" lhs rhs
  prettyPrintSimple (InstructionDIVSD lhs rhs) = printBinOp "divsd" lhs rhs
  prettyPrintSimple (InstructionCOMISD lhs rhs) = printBinOp "comisd" lhs rhs
  prettyPrintSimple (InstructionMOVSD_XMM_XMM lhs rhs) =
    printBinOp "movsd" lhs rhs
  prettyPrintSimple (InstructionMOVSD_XMM_M64 lhs rhs) =
    printBinOp "movsd" lhs rhs
  prettyPrintSimple (InstructionMOVSD_M64_XMM lhs rhs) =
    printBinOp "movsd" lhs rhs
  prettyPrintSimple (InstructionCVTSI2SD lhs rhs) =
    printBinOp "cvtsi2sd" lhs rhs
  prettyPrintSimple (InstructionPUSH v) = printUnOp "push" v
  prettyPrintSimple (InstructionPOP v) = printUnOp "pop" v
  prettyPrintSimple (InstructionLEA r s) =
    "lea " ++ prettyPrintSimple r ++ ", [rip + " ++ show s ++ "]"

{-# INLINE cast #-}
cast ::
     (MArray (STUArray s) a (ST s), MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0

doubleToWord64 :: Double -> Word64
doubleToWord64 x = runST (cast x)

instance PrettyPrintSimple Immediate where
  prettyPrintSimple (ImmediateInt i) = show i
  prettyPrintSimple (ImmediateFloat f) = show $ doubleToWord64 f
