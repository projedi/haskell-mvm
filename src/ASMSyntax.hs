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
  , Operand(..)
  , operandType
  , Expr(ExprFunctionCall, ExprRead, ExprPeekStack, ExprDereference,
     ExprAddressOf, ExprConst, ExprBinOp, ExprUnOp)
  , exprType
  , functionCallType
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

data Operand
  = OperandVar Var
  | OperandRegister VarType
                    Register

operandType :: Operand -> VarType
operandType (OperandVar v) = varType v
operandType (OperandRegister t _) = t

data Statement
  = StatementFunctionCall FunctionCall
  | StatementAssign Operand
                    Expr
  | StatementAssignToPtr Operand
                         Operand
  | StatementPushOnStack Operand
  | StatementAllocateOnStack VarType
  | StatementPopFromStack
  | StatementReturn (Maybe Operand)
  | StatementLabel LabelID
  | StatementJump LabelID
  | StatementJumpIfZero Operand
                        LabelID

data FunctionDef = FunctionDef
  { funDefRetType :: Maybe VarType
  , funDefName :: FunID
  , funDefParams :: [Var]
  , funDefLocals :: [Var]
  , funDefBody :: [Statement]
  , funDefBeforeBody :: [Statement]
  , funDefAfterBody :: [Statement]
  }

data FunctionCall
  = NativeFunctionCall { nativeFunCallName :: FunID
                       , nativeFunCallRetType :: Maybe VarType
                       , nativeFunCallArgs :: [Operand] }
  | ForeignFunctionCall { foreignFunCallName :: FunID
                        , foreignFunCallRetType :: Maybe VarType
                        , foreignFunCallArgs :: [Operand] }

functionCallType :: FunctionCall -> Maybe VarType
functionCallType NativeFunctionCall {nativeFunCallRetType = rettype} = rettype
functionCallType ForeignFunctionCall {foreignFunCallRetType = rettype} = rettype

data Var = Var
  { varType :: VarType
  , varDisplacement :: Int64 -- Displacement from RBP.
  }

data Expr = Expr
  { exprType :: VarType
  , exprImpl :: ExprImpl
  }

data ExprImpl
  = ExprFunctionCallImpl FunctionCall
  | ExprReadImpl Operand
  | ExprPeekStackImpl
  | ExprDereferenceImpl Operand
  | ExprAddressOfImpl Var
  | ExprConstImpl ConstID
  | ExprBinOpImpl BinOp
                  Operand
                  Operand
  | ExprUnOpImpl UnOp
                 Operand

pattern ExprFunctionCall :: FunctionCall -> Expr

pattern ExprFunctionCall fcall <-
        Expr{exprImpl = ExprFunctionCallImpl fcall}
  where ExprFunctionCall fcall
          = Expr{exprType = t, exprImpl = ExprFunctionCallImpl fcall}
          where Just t = functionCallType fcall

pattern ExprRead :: Operand -> Expr

pattern ExprRead x <- Expr{exprImpl = ExprReadImpl x}
  where ExprRead x
          = Expr{exprType = operandType x, exprImpl = ExprReadImpl x}

pattern ExprPeekStack :: VarType -> Expr

pattern ExprPeekStack vType =
        Expr{exprType = vType, exprImpl = ExprPeekStackImpl}

pattern ExprDereference :: Operand -> Expr

pattern ExprDereference x <- Expr{exprImpl = ExprDereferenceImpl x}
  where ExprDereference x
          = Expr{exprType = t, exprImpl = ExprDereferenceImpl x}
          where (VarTypePtr t) = operandType x

pattern ExprAddressOf :: Var -> Expr

pattern ExprAddressOf v <- Expr{exprImpl = ExprAddressOfImpl v}
  where ExprAddressOf v
          = Expr{exprType = VarTypePtr (varType v),
                 exprImpl = ExprAddressOfImpl v}

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

{-# COMPLETE ExprFunctionCall, ExprRead, ExprPeekStack,
  ExprDereference, ExprAddressOf, ExprConst, ExprBinOp, ExprUnOp
  :: Expr #-}
