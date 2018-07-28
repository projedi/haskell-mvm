{-# LANGUAGE PatternSynonyms #-}

module LinearSyntax
  ( Program(..)
  , VarID(..)
  , FunID(..)
  , ConstID(..)
  , LabelID(..)
  , VarType(..)
  , VarDecl(..)
  , Statement(..)
  , FunctionDef(..)
  , ForeignFunctionDecl(..)
  , FunctionCall(..)
  , BinOp(..)
  , binOpTypeFromArgs
  , UnOp(..)
  , unOpTypeFromArg
  , Var(..)
  , Expr(ExprFunctionCall, ExprVar, ExprDereference, ExprAddressOf,
     ExprConst, ExprBinOp, ExprUnOp)
  , exprType
  , functionCallType
  ) where

import Data.IntMap (IntMap)

import SimplifiedSyntax
  ( BinOp(..)
  , ConstID(..)
  , ForeignFunctionDecl(..)
  , FunID(..)
  , UnOp(..)
  , VarDecl(..)
  , VarID(..)
  , VarType(..)
  , binOpTypeFromArgs
  , unOpTypeFromArg
  )
import Value (Value)

newtype LabelID =
  LabelID Int

instance Show LabelID where
  show (LabelID l) = "label_" ++ show l

data Program = Program
  { programFunctions :: IntMap FunctionDef
  , programLibraries :: [String]
  , programForeignFunctions :: IntMap ForeignFunctionDecl
  , programConstants :: IntMap Value
  , programVariables :: IntMap VarType
  , programLastFunID :: FunID
  , programLastVarID :: VarID
  , programLastConstID :: ConstID
  , programLastLabelID :: LabelID
  }

data Statement
  = StatementFunctionCall FunctionCall
  | StatementAssign VarID
                    Expr
  | StatementAssignToPtr VarID
                         Expr
  | StatementReturn (Maybe Expr)
  | StatementLabel LabelID
  | StatementJump LabelID
  | StatementJumpIfZero Expr
                        LabelID

data FunctionDef = FunctionDef
  { funDefRetType :: Maybe VarType
  , funDefName :: FunID
  , funDefParams :: [VarDecl]
  , funDefLocals :: [VarID]
  , funDefBody :: [Statement]
  }

data FunctionCall
  = NativeFunctionCall { nativeFunCallName :: FunID
                       , nativeFunCallRetType :: Maybe VarType
                       , nativeFunCallArgs :: [Expr] }
  | ForeignFunctionCall { foreignFunCallName :: FunID
                        , foreignFunCallRetType :: Maybe VarType
                        , foreignFunCallArgs :: [Expr] }

functionCallType :: FunctionCall -> Maybe VarType
functionCallType NativeFunctionCall {nativeFunCallRetType = rettype} = rettype
functionCallType ForeignFunctionCall {foreignFunCallRetType = rettype} = rettype

data Var = Var
  { varName :: VarID
  , varType :: VarType
  }

data Expr = Expr
  { exprType :: VarType
  , exprImpl :: ExprImpl
  }

data ExprImpl
  = ExprFunctionCallImpl FunctionCall
  | ExprVarImpl VarID
  | ExprDereferenceImpl VarID
  | ExprAddressOfImpl VarID
  | ExprConstImpl ConstID
  | ExprBinOpImpl BinOp
                  Expr
                  Expr
  | ExprUnOpImpl UnOp
                 Expr

pattern ExprFunctionCall :: FunctionCall -> Expr

pattern ExprFunctionCall fcall <-
        Expr{exprImpl = ExprFunctionCallImpl fcall}
  where ExprFunctionCall fcall
          = Expr{exprType = t, exprImpl = ExprFunctionCallImpl fcall}
          where Just t = functionCallType fcall

pattern ExprVar :: VarType -> VarID -> Expr

pattern ExprVar vType v =
        Expr{exprType = vType, exprImpl = ExprVarImpl v}

pattern ExprDereference :: VarType -> VarID -> Expr

pattern ExprDereference vType v =
        Expr{exprType = vType, exprImpl = ExprDereferenceImpl v}

pattern ExprAddressOf :: VarType -> VarID -> Expr

pattern ExprAddressOf vType v =
        Expr{exprType = vType, exprImpl = ExprAddressOfImpl v}

pattern ExprConst :: VarType -> ConstID -> Expr

pattern ExprConst vType cid =
        Expr{exprType = vType, exprImpl = ExprConstImpl cid}

pattern ExprBinOp :: BinOp -> Expr -> Expr -> Expr

pattern ExprBinOp op lhs rhs <-
        Expr{exprImpl = ExprBinOpImpl op lhs rhs}
  where ExprBinOp op lhs rhs
          = Expr{exprType = t, exprImpl = ExprBinOpImpl op lhs rhs}
          where t = binOpTypeFromArgs op (exprType lhs) (exprType rhs)

pattern ExprUnOp :: UnOp -> Expr -> Expr

pattern ExprUnOp op e <- Expr{exprImpl = ExprUnOpImpl op e}
  where ExprUnOp op e
          = Expr{exprType = t, exprImpl = ExprUnOpImpl op e}
          where t = unOpTypeFromArg op $ exprType e

{-# COMPLETE ExprFunctionCall, ExprVar, ExprDereference,
  ExprAddressOf, ExprConst, ExprBinOp, ExprUnOp :: Expr #-}
