{-# LANGUAGE PatternSynonyms #-}

module ASMSyntax
  ( Program(..)
  , FunID(..)
  , ConstID(..)
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
  , Var(..)
  , Register(..)
  , Pointer(..)
  , Operand(..)
  , operandType
  , Expr(ExprRead, ExprDereference, ExprConst, ExprBinOp, ExprUnOp)
  , exprType
  ) where

import Data.Int (Int64)
import Data.IntMap (IntMap)

import LinearSyntax
  ( BinOp(..)
  , ConstID(..)
  , ForeignFunctionDecl(..)
  , FunID(..)
  , LabelID(..)
  , UnOp(..)
  , VarType(..)
  , binOpTypeFromArgs
  , unOpTypeFromArg
  )
import Value (Value)

data Program = Program
  { programFunctions :: IntMap FunctionDef
  , programLibraries :: [String]
  , programForeignFunctions :: IntMap ForeignFunctionDecl
  , programConstants :: IntMap Value
  , programLastFunID :: FunID
  , programLastConstID :: ConstID
  , programLastLabelID :: LabelID
  }

data Register
  = RegisterRSP
  | RegisterRBP
  | RegisterRAX
  | RegisterXMM0

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
  | StatementAssign Operand
                    Expr
  | StatementAssignToPtr Operand
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
  , funDefAfterBody :: [Statement]
  }

data FunctionCall
  = NativeFunctionCall { nativeFunCallName :: FunID }
  | ForeignFunctionCall { foreignFunCallName :: FunID
                        , foreignFunCallRetType :: Maybe VarType
                        , foreignFunCallArgTypes :: [VarType] }

data Var = Var
  { varType :: VarType
  , varDisplacement :: Int64 -- Displacement from RBP.
  }

data Expr = Expr
  { exprType :: VarType
  , exprImpl :: ExprImpl
  }

data ExprImpl
  = ExprReadImpl Operand
  | ExprDereferenceImpl Operand
  | ExprConstImpl ConstID
  | ExprBinOpImpl BinOp
                  Operand
                  Operand
  | ExprUnOpImpl UnOp
                 Operand

pattern ExprRead :: Operand -> Expr

pattern ExprRead x <- Expr{exprImpl = ExprReadImpl x}
  where ExprRead x
          = Expr{exprType = operandType x, exprImpl = ExprReadImpl x}

pattern ExprDereference :: Operand -> Expr

pattern ExprDereference x <- Expr{exprImpl = ExprDereferenceImpl x}
  where ExprDereference x
          = Expr{exprType = t, exprImpl = ExprDereferenceImpl x}
          where (VarTypePtr t) = operandType x

pattern ExprConst :: VarType -> ConstID -> Expr

pattern ExprConst cType cid =
        Expr{exprType = cType, exprImpl = ExprConstImpl cid}

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

{-# COMPLETE ExprRead, ExprDereference, ExprConst, ExprBinOp,
  ExprUnOp :: Expr #-}
