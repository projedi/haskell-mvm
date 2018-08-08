{-# LANGUAGE PatternSynonyms #-}

module ASMSyntax
  ( Program(..)
  , FunID(..)
  , StringID(..)
  , LabelID(..)
  , VarType(..)
  , Statement(..)
  , FunctionDef(..)
  , ForeignFunctionDecl(..)
  , FunctionCall(..)
  , BinOp(..)
  , binOpTypeFromArgs
  , UnOp(..)
  , unOpTypeFromArg
  , Register(..)
  , Pointer(..)
  , Operand(..)
  , operandType
  , Immediate(..)
  , immediateType
  ) where

import Data.Int (Int64)
import Data.IntMap (IntMap)

import LinearSyntax
  ( ForeignFunctionDecl(..)
  , FunID(..)
  , Immediate(..)
  , LabelID(..)
  , StringID(..)
  , VarType(..)
  , immediateType
  )

data Program = Program
  { programCode :: FunctionDef
  , programLibraries :: [String]
  , programForeignFunctions :: IntMap ForeignFunctionDecl
  , programStrings :: IntMap String
  , programLastFunID :: FunID
  , programLastStringID :: StringID
  , programLastLabelID :: LabelID
  }

data Register
  = RegisterRSP
  | RegisterRBP
  | RegisterRAX
  | RegisterRDI
  | RegisterRSI
  | RegisterRDX
  | RegisterRCX
  | RegisterR8
  | RegisterR9
  | RegisterXMM0
  | RegisterXMM1
  | RegisterXMM2
  | RegisterXMM3
  | RegisterXMM4
  | RegisterXMM5
  | RegisterXMM6
  | RegisterXMM7

data Pointer = Pointer
  { pointerType :: VarType
  , pointerBase :: Maybe Register
  , pointerDisplacement :: Int64
  }

data Operand
  = OperandRegister VarType
                    Register
  | OperandPointer Pointer

operandType :: Operand -> VarType
operandType (OperandRegister t _) = t
operandType (OperandPointer p) = pointerType p

data UnOp
  = UnNegFloat
  | UnIntToFloat

unOpTypeFromArg :: UnOp -> VarType -> VarType
unOpTypeFromArg UnNegFloat VarTypeFloat = VarTypeFloat
unOpTypeFromArg UnNegFloat _ = error "Type mismatch"
unOpTypeFromArg UnIntToFloat VarTypeInt = VarTypeFloat
unOpTypeFromArg UnIntToFloat _ = error "Type mismatch"

data BinOp
  = BinPlusFloat
  | BinMinusFloat
  | BinTimesFloat
  | BinDivFloat
  | BinEqFloat
  | BinLtFloat

binOpTypeFromArgs :: BinOp -> VarType -> VarType -> VarType
binOpTypeFromArgs BinPlusFloat VarTypeFloat VarTypeFloat = VarTypeFloat
binOpTypeFromArgs BinPlusFloat _ _ = error "Type mismatch"
binOpTypeFromArgs BinMinusFloat VarTypeFloat VarTypeFloat = VarTypeFloat
binOpTypeFromArgs BinMinusFloat _ _ = error "Type mismatch"
binOpTypeFromArgs BinTimesFloat VarTypeFloat VarTypeFloat = VarTypeFloat
binOpTypeFromArgs BinTimesFloat _ _ = error "Type mismatch"
binOpTypeFromArgs BinDivFloat VarTypeFloat VarTypeFloat = VarTypeFloat
binOpTypeFromArgs BinDivFloat _ _ = error "Type mismatch"
binOpTypeFromArgs BinEqFloat VarTypeFloat VarTypeFloat = VarTypeInt
binOpTypeFromArgs BinEqFloat _ _ = error "Type mismatch"
binOpTypeFromArgs BinLtFloat VarTypeFloat VarTypeFloat = VarTypeInt
binOpTypeFromArgs BinLtFloat _ _ = error "Type mismatch"

data Statement
  -- Stores result in RAX
  = StatementBinOp BinOp
                   Operand
                   Operand
  -- Stores result in RAX
  | StatementUnOp UnOp
                  Operand
  | StatementPushOnStack Operand
  | StatementAllocateOnStack VarType
  | StatementPopFromStack VarType
  --
  -- From here on, statements are directly representable as ASM instructions.
  --
  -- Subtract one from the other and set EFLAGS accordingly.
  | InstructionCMP Operand
                   Operand
  -- Set to 1 if ZF(EFLAGS) = 1, 0 - otherwise.
  | InstructionSetZ Operand
  -- Set to 1 if ZF(EFLAGS) = 0, 0 - otherwise.
  | InstructionSetNZ Operand
  -- Set to 1 if SF(EFLAGS) = 1, 0 - otherwise.
  | InstructionSetS Operand
  -- Copy from rhs to lhs.
  | InstructionMOV Operand
                   (Either Operand Immediate)
  -- A nop that has a label attached.
  | InstructionLabelledNOP LabelID
  -- Unconditional jump.
  | InstructionJMP LabelID
  -- Jump if ZF(EFLAGS) = 1
  | InstructionJZ LabelID
  -- Pop RIP from the stack and jump to it.
  | InstructionRET
  -- Push RIP to the stack and jump.
  | InstructionCALL FunctionCall
  -- Negate integer operand
  | InstructionNEG Operand
  -- Bitwise AND instruction. Stores result in the lhs.
  | InstructionAND Operand
                   Operand
  -- Bitwise XOR instruction. Stores result in the lhs.
  | InstructionXOR Operand
                   Operand
  -- Bitwise OR instruction. Stores result in the lhs.
  | InstructionOR Operand
                  Operand
  -- lhs + rhs. Stores result in the lhs.
  | InstructionADD Operand
                   Operand
  -- lhs - rhs. Stores result in the lhs.
  | InstructionSUB Operand
                   Operand
  -- Divides RDX:RAX by operand. Stores result quotient in RAX, remainder in RDX.
  | InstructionIDIV Operand
  -- lhs * rhs. Stores result in lhs.
  | InstructionIMUL Operand
                    Operand
  -- Sign extends RAX into RDX:RAX.
  | InstructionCQO

data FunctionDef = FunctionDef
  { funDefBody :: [Statement]
  }

data FunctionCall
  = NativeFunctionCall { nativeFunCallName :: LabelID }
  | ForeignFunctionCall { foreignFunCallName :: FunID
                        , foreignFunCallRetType :: Maybe VarType
                        , foreignFunCallArgTypes :: [VarType] }
