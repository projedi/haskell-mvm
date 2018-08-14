{-# LANGUAGE PatternSynonyms #-}

module ASMSyntax
  ( Program(..)
  , FunID(..)
  , StringID(..)
  , LabelID(..)
  , VarType(..)
  , Instruction(..)
  , ForeignFunctionDecl(..)
  , FunctionCall(..)
  , Register(..)
  , Register8(..)
  , RegisterXMM(..)
  , Pointer(..)
  , IntOperand(..)
  , intOperandType
  , Immediate(..)
  , immediateType
  , typeSize
  , typesSize
  ) where

import Data.Int (Int64)
import Data.IntMap (IntMap)

import LinearSyntax
  ( ForeignFunctionDecl(..)
  , FunID(..)
  , LabelID(..)
  , StringID(..)
  , VarType(..)
  )

data Program = Program
  { programCode :: [Instruction]
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
  | RegisterRBX
  | RegisterRDI
  | RegisterRSI
  | RegisterRDX
  | RegisterRCX
  | RegisterR8
  | RegisterR9

data Register8
  = RegisterAL
  | RegisterCL
  | RegisterDL

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
  , pointerBase :: Register
  , pointerDisplacement :: Int64
  }

data IntOperand
  = IntOperandRegister VarType
                       Register
  | IntOperandPointer Pointer

intOperandType :: IntOperand -> VarType
intOperandType (IntOperandRegister t _) = t
intOperandType (IntOperandPointer p) = pointerType p

data Immediate
  = ImmediateInt Int64
  | ImmediateFloat Double

immediateType :: Immediate -> VarType
immediateType (ImmediateInt _) = VarTypeInt
immediateType (ImmediateFloat _) = VarTypeFloat

data Instruction
  -- Subtract one from the other and set EFLAGS accordingly.
  = InstructionCMP Register
                   IntOperand
  -- Set to 1 if ZF(EFLAGS) = 1, 0 - otherwise.
  | InstructionSetZ Register8
  -- Set to 1 if ZF(EFLAGS) = 0, 0 - otherwise.
  | InstructionSetNZ Register8
  -- Set to 1 if SF(EFLAGS) = 1, 0 - otherwise.
  | InstructionSetS Register8
  -- Set to 1 if CF(EFLAGS) = 1, 0 - otherwise.
  | InstructionSetC Register8
  -- Copy from rhs to lhs.
  | InstructionMOV_R64_IMM64 Register
                             Immediate
  | InstructionMOV_RM64_R64 IntOperand
                            Register
  | InstructionMOV_R64_RM64 Register
                            IntOperand
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
  | InstructionAND Register
                   IntOperand
  -- Bitwise XOR instruction. Stores result in the lhs.
  | InstructionXOR Register
                   IntOperand
  -- Bitwise OR instruction. Stores result in the lhs.
  | InstructionOR Register
                  IntOperand
  -- lhs + rhs. Stores result in the lhs.
  | InstructionADD Register
                   IntOperand
  -- lhs - rhs. Stores result in the lhs.
  | InstructionSUB Register
                   IntOperand
  -- Divides RDX:RAX by operand. Stores result quotient in RAX, remainder in RDX.
  | InstructionIDIV IntOperand
  -- lhs * rhs. Stores result in lhs.
  | InstructionIMUL Register
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
  -- Push operand on stack. Adjusts RSP.
  | InstructionPUSH Register
  -- Pop from stack onto operand. Adjusts RSP.
  | InstructionPOP Register
  -- Store address of string in register
  | InstructionLEA Register
                   StringID

data FunctionCall
  = NativeFunctionCall { nativeFunCallName :: LabelID }
  | ForeignFunctionCall { foreignFunCallName :: FunID
                        , foreignFunCallRealName :: String
                        , foreignFunCallRetType :: Maybe VarType
                        , foreignFunCallArgTypes :: [VarType] }

typeSize :: VarType -> Int64
typeSize _ = 8

typesSize :: [VarType] -> Int64
typesSize = sum . map typeSize
