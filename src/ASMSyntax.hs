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
  , Expr(ExprConst, ExprBinOp, ExprUnOp)
  , exprType
  , Immediate(..)
  , immediateType
  ) where

import Data.Int (Int64)
import Data.IntMap (IntMap)

import LinearSyntax
  ( BinOp(..)
  , ForeignFunctionDecl(..)
  , FunID(..)
  , Immediate(..)
  , LabelID(..)
  , StringID(..)
  , UnOp(..)
  , VarType(..)
  , binOpTypeFromArgs
  , immediateType
  , unOpTypeFromArg
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
  | OperandImmediateInt Int64 -- TODO: Reconsider.

operandType :: Operand -> VarType
operandType (OperandRegister t _) = t
operandType (OperandPointer p) = pointerType p
operandType (OperandImmediateInt _) = VarTypeInt

data Statement
  = StatementFunctionCall FunctionCall
  | StatementExpr Expr -- stores result in RAX
  | StatementAssign Operand
                    Operand
  | StatementPushOnStack Operand
  | StatementAllocateOnStack VarType
  | StatementPopFromStack VarType
  | StatementReturn
  | StatementLabel LabelID
  | StatementJump LabelID
  | StatementJumpIfZero Operand
                        LabelID

data FunctionDef = FunctionDef
  { funDefBody :: [Statement]
  }

data FunctionCall
  = NativeFunctionCall { nativeFunCallName :: LabelID }
  | ForeignFunctionCall { foreignFunCallName :: FunID
                        , foreignFunCallRetType :: Maybe VarType
                        , foreignFunCallArgTypes :: [VarType] }

data Expr = Expr
  { exprType :: VarType
  , exprImpl :: ExprImpl
  }

data ExprImpl
  = ExprConstImpl Immediate
  | ExprBinOpImpl BinOp
                  Operand
                  Operand
  | ExprUnOpImpl UnOp
                 Operand

pattern ExprConst :: Immediate -> Expr

pattern ExprConst imm <- Expr{exprImpl = ExprConstImpl imm}
  where ExprConst imm
          = Expr{exprType = immediateType imm, exprImpl = ExprConstImpl imm}

pattern ExprBinOp :: BinOp -> Operand -> Operand -> Expr

pattern ExprBinOp op lhs rhs <-
        Expr{exprImpl = ExprBinOpImpl op lhs rhs}
  where ExprBinOp op lhs rhs
          = Expr{exprType = t, exprImpl = ExprBinOpImpl op lhs rhs}
          where t = binOpTypeFromArgs op (operandType lhs) (operandType rhs)

pattern ExprUnOp :: UnOp -> Operand -> Expr

pattern ExprUnOp op x <- Expr{exprImpl = ExprUnOpImpl op x}
  where ExprUnOp op x
          = Expr{exprType = t, exprImpl = ExprUnOpImpl op x}
          where t = unOpTypeFromArg op $ operandType x

{-# COMPLETE ExprConst, ExprBinOp, ExprUnOp :: Expr #-}
