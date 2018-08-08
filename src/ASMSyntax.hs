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
  , Register(..)
  , RegisterXMM(..)
  , Pointer(..)
  , IntOperand(..)
  , intOperandType
  , FloatOperand(..)
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

data RegisterXMM
  = RegisterXMM0
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

data IntOperand
  = IntOperandRegister VarType
                       Register
  | IntOperandPointer Pointer

intOperandType :: IntOperand -> VarType
intOperandType (IntOperandRegister t _) = t
intOperandType (IntOperandPointer p) = pointerType p

data FloatOperand
  = FloatOperandRegister RegisterXMM
  | FloatOperandPointer Pointer

data BinOp
  = BinPlusFloat
  | BinMinusFloat
  | BinTimesFloat

binOpTypeFromArgs :: BinOp -> VarType -> VarType -> VarType
binOpTypeFromArgs BinPlusFloat VarTypeFloat VarTypeFloat = VarTypeFloat
binOpTypeFromArgs BinPlusFloat _ _ = error "Type mismatch"
binOpTypeFromArgs BinMinusFloat VarTypeFloat VarTypeFloat = VarTypeFloat
binOpTypeFromArgs BinMinusFloat _ _ = error "Type mismatch"
binOpTypeFromArgs BinTimesFloat VarTypeFloat VarTypeFloat = VarTypeFloat
binOpTypeFromArgs BinTimesFloat _ _ = error "Type mismatch"

data Statement
  -- Stores result in XMM0
  = StatementBinOp BinOp
                   FloatOperand
                   FloatOperand
  | StatementPushOnStack IntOperand
  | StatementAllocateOnStack VarType
  | StatementPopFromStack VarType
  --
  -- From here on, statements are directly representable as ASM instructions.
  --
  -- Subtract one from the other and set EFLAGS accordingly.
  | InstructionCMP IntOperand
                   IntOperand
  -- Set to 1 if ZF(EFLAGS) = 1, 0 - otherwise.
  | InstructionSetZ IntOperand
  -- Set to 1 if ZF(EFLAGS) = 0, 0 - otherwise.
  | InstructionSetNZ IntOperand
  -- Set to 1 if SF(EFLAGS) = 1, 0 - otherwise.
  | InstructionSetS IntOperand
  -- Set to 1 if CF(EFLAGS) = 1, 0 - otherwise.
  | InstructionSetC IntOperand
  -- Copy from rhs to lhs.
  | InstructionMOV IntOperand
                   (Either IntOperand Immediate)
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
  | InstructionNEG IntOperand
  -- Bitwise AND instruction. Stores result in the lhs.
  | InstructionAND IntOperand
                   IntOperand
  -- Bitwise XOR instruction. Stores result in the lhs.
  | InstructionXOR IntOperand
                   IntOperand
  -- Bitwise OR instruction. Stores result in the lhs.
  | InstructionOR IntOperand
                  IntOperand
  -- lhs + rhs. Stores result in the lhs.
  | InstructionADD IntOperand
                   IntOperand
  -- lhs - rhs. Stores result in the lhs.
  | InstructionSUB IntOperand
                   IntOperand
  -- Divides RDX:RAX by operand. Stores result quotient in RAX, remainder in RDX.
  | InstructionIDIV IntOperand
  -- lhs * rhs. Stores result in lhs.
  | InstructionIMUL IntOperand
                    IntOperand
  -- Sign extends RAX into RDX:RAX.
  | InstructionCQO
  -- Add low double precision in lhs to low double precision in rhs and store in rhs.
  | InstructionADDSD RegisterXMM
                     RegisterXMM
  -- Subtract low double precision in rhs from low double precision in lhs and store in lhs.
  | InstructionSUBSD RegisterXMM
                     RegisterXMM
  -- Multiply low double precision in lhs by low double precision in rhs and store in lhs.
  | InstructionMULSD RegisterXMM
                     RegisterXMM
  -- Divide low double precision in lhs by low double precision in rhs and store in lhs.
  | InstructionDIVSD RegisterXMM
                     RegisterXMM
  -- Compare low double precision in lhs with low double precision in rhs and set EFLAGS.
  | InstructionCOMISD RegisterXMM
                      RegisterXMM
  -- Move from rhs to lhs.
  | InstructionMOVSD_XMM_XMM RegisterXMM
                             RegisterXMM
  | InstructionMOVSD_XMM_M64 RegisterXMM
                             Pointer
  | InstructionMOVSD_M64_XMM Pointer
                             RegisterXMM
  -- Convert from integer in rhs to double precision in lhs.
  | InstructionCVTSI2SD RegisterXMM
                        IntOperand

data FunctionDef = FunctionDef
  { funDefBody :: [Statement]
  }

data FunctionCall
  = NativeFunctionCall { nativeFunCallName :: LabelID }
  | ForeignFunctionCall { foreignFunCallName :: FunID
                        , foreignFunCallRetType :: Maybe VarType
                        , foreignFunCallArgTypes :: [VarType] }
