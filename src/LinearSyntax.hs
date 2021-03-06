{-# LANGUAGE PatternSynonyms #-}

module LinearSyntax
  ( Program(..)
  , VarID(..)
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
  , Var(..)
  , Immediate(..)
  , immediateType
  , Expr(ExprFunctionCall, ExprVar, ExprDereference, ExprAddressOf,
     ExprConst, ExprBinOp, ExprUnOp)
  , exprType
  , functionCallType
  ) where

import Data.IntMap (IntMap)

import SimplifiedSyntax
  ( BinOp(..)
  , ForeignFunctionDecl(..)
  , FunID(..)
  , Immediate(..)
  , StringID(..)
  , UnOp(..)
  , VarID(..)
  , VarType(..)
  , binOpTypeFromArgs
  , immediateType
  , unOpTypeFromArg
  )

newtype LabelID =
  LabelID Int

instance Show LabelID where
  show (LabelID l) = "label_" ++ show l

data Program = Program
  { programFunctions :: IntMap FunctionDef
  , programLibraries :: [String]
  , programForeignFunctions :: IntMap ForeignFunctionDecl
  , programStrings :: IntMap String
  , programVariables :: IntMap VarType
  , programLastFunID :: FunID
  , programLastVarID :: VarID
  , programLastStringID :: StringID
  , programLastLabelID :: LabelID
  }

data Statement
  = StatementFunctionCall FunctionCall
  | StatementAssign Var
                    Expr
  | StatementAssignToPtr Var
                         Var
  | StatementReturn (Maybe Var)
  | StatementLabel LabelID
  | StatementJump LabelID
  | StatementJumpIfZero Var
                        LabelID

data FunctionDef = FunctionDef
  { funDefRetType :: Maybe VarType
  , funDefName :: FunID
  , funDefParams :: [Var]
  , funDefLocals :: [Var]
  , funDefBody :: [Statement]
  }

data FunctionCall
  = NativeFunctionCall { nativeFunCallName :: FunID
                       , nativeFunCallRetType :: Maybe VarType
                       , nativeFunCallArgs :: [Var] }
  | ForeignFunctionCall { foreignFunCallName :: FunID
                        , foreignFunCallRetType :: Maybe VarType
                        , foreignFunCallArgs :: [Var] }

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
  | ExprVarImpl Var
  | ExprDereferenceImpl Var
  | ExprAddressOfImpl Var
  | ExprConstImpl Immediate
  | ExprBinOpImpl BinOp
                  Var
                  Var
  | ExprUnOpImpl UnOp
                 Var

pattern ExprFunctionCall :: FunctionCall -> Expr

pattern ExprFunctionCall fcall <-
        Expr{exprImpl = ExprFunctionCallImpl fcall}
  where ExprFunctionCall fcall
          = Expr{exprType = t, exprImpl = ExprFunctionCallImpl fcall}
          where Just t = functionCallType fcall

pattern ExprVar :: Var -> Expr

pattern ExprVar v <- Expr{exprImpl = ExprVarImpl v}
  where ExprVar v
          = Expr{exprType = varType v, exprImpl = ExprVarImpl v}

pattern ExprDereference :: Var -> Expr

pattern ExprDereference v <- Expr{exprImpl = ExprDereferenceImpl v}
  where ExprDereference v
          = Expr{exprType = t, exprImpl = ExprDereferenceImpl v}
          where (VarTypePtr t) = varType v

pattern ExprAddressOf :: Var -> Expr

pattern ExprAddressOf v <- Expr{exprImpl = ExprAddressOfImpl v}
  where ExprAddressOf v
          = Expr{exprType = VarTypePtr (varType v),
                 exprImpl = ExprAddressOfImpl v}

pattern ExprConst :: Immediate -> Expr

pattern ExprConst imm <- Expr{exprImpl = ExprConstImpl imm}
  where ExprConst imm
          = Expr{exprType = immediateType imm, exprImpl = ExprConstImpl imm}

pattern ExprBinOp :: BinOp -> Var -> Var -> Expr

pattern ExprBinOp op lhs rhs <-
        Expr{exprImpl = ExprBinOpImpl op lhs rhs}
  where ExprBinOp op lhs rhs
          = Expr{exprType = t, exprImpl = ExprBinOpImpl op lhs rhs}
          where t = binOpTypeFromArgs op (varType lhs) (varType rhs)

pattern ExprUnOp :: UnOp -> Var -> Expr

pattern ExprUnOp op v <- Expr{exprImpl = ExprUnOpImpl op v}
  where ExprUnOp op v
          = Expr{exprType = t, exprImpl = ExprUnOpImpl op v}
          where t = unOpTypeFromArg op $ varType v

{-# COMPLETE ExprFunctionCall, ExprVar, ExprDereference,
  ExprAddressOf, ExprConst, ExprBinOp, ExprUnOp :: Expr #-}
