{-# LANGUAGE PatternSynonyms #-}
module Syntax
  ( Program(..)
  , VarID(..)
  , FunID(..)
  , VarType(..)
  , VarDecl(..)
  , Block(..)
  , Statement(..)
  , AccessRecorder(..)
  , FunctionDef(..)
  , ForeignFunctionDecl(..)
  , FunctionCall(..)
  , NumVarType(NumVarTypeInt, NumVarTypeFloat)
  , numTypeToType
  , BinOp(..)
  , UnOp(..)
  , Expr
    ( ExprFunctionCall
    , ExprVar
    , ExprInt
    , ExprFloat
    , ExprString
    , ExprBinOp
    , ExprUnOp
    )
  , exprType
  , functionCallType
  ) where

import Data.IntMap (IntMap)

import ResolvedSyntax
       (VarType(..), VarID(..), FunID(..), VarDecl(..),
        AccessRecorder(..), ForeignFunctionDecl(..))

data Program = Program
  { programFunctions :: IntMap FunctionDef
  , programLibraries :: [String]
  , programForeignFunctions :: IntMap ForeignFunctionDecl
  , programLastFunID :: FunID
  , programLastVarID :: VarID
  }

data Block = Block
  { blockVariables :: [VarDecl]
  , blockStatements :: [Statement]
  }

data Statement
  = StatementBlock Block
  | StatementFunctionCall FunctionCall
  | StatementWhile Expr
                   Block
  | StatementAssign VarID
                    Expr
  | StatementIfElse Expr
                    Block
                    Block
  | StatementReturn (Maybe Expr)

data FunctionDef = FunctionDef
  { funDefRetType :: Maybe VarType
  , funDefName :: FunID
  , funDefParams :: [VarDecl]
  , funDefAccesses :: AccessRecorder
  , funDefBody :: Block
  }

data FunctionCall
  = NativeFunctionCall
    { nativeFunCallName :: FunID
    , nativeFunCallRetType :: Maybe VarType
    , nativeFunCallArgs :: [Expr]
    }
  | ForeignFunctionCall
    { foreignFunCallName :: FunID
    , foreignFunCallRetType :: Maybe VarType
    , foreignFunCallArgs :: [Expr]
    }
  | PrintCall [Expr]

functionCallType :: FunctionCall -> Maybe VarType
functionCallType NativeFunctionCall{ nativeFunCallRetType = rettype } = rettype
functionCallType ForeignFunctionCall{ foreignFunCallRetType = rettype } = rettype
functionCallType (PrintCall _) = Nothing

newtype NumVarType = NumVarType { numTypeToType :: VarType }

pattern NumVarTypeInt :: NumVarType
pattern NumVarTypeInt = NumVarType VarTypeInt

pattern NumVarTypeFloat :: NumVarType
pattern NumVarTypeFloat = NumVarType VarTypeFloat

data BinOp
  = BinPlus
  | BinMinus
  | BinTimes
  | BinDiv
  | BinMod
  | BinBitAnd
  | BinBitOr
  | BinBitXor
  | BinAnd
  | BinOr
  | BinEq
  | BinLt

data UnOp
  = UnNeg
  | UnNot
  | UnIntToFloat

data Expr = Expr
  { exprType :: VarType
  , exprImpl :: ExprImpl
  }

data ExprImpl
  = ExprFunctionCallImpl FunctionCall
  | ExprVarImpl VarID
  | ExprIntImpl Int
  | ExprFloatImpl Double
  | ExprStringImpl String
  | ExprBinOpImpl BinOp Expr Expr
  | ExprUnOpImpl UnOp Expr

pattern ExprFunctionCall :: FunctionCall -> Expr
pattern ExprFunctionCall fcall <- Expr { exprImpl = ExprFunctionCallImpl fcall } where
  ExprFunctionCall fcall = Expr { exprType = t, exprImpl = ExprFunctionCallImpl fcall }
    where
      Just t = functionCallType fcall

pattern ExprVar :: VarType -> VarID -> Expr
pattern ExprVar vType v = Expr { exprType = vType, exprImpl = ExprVarImpl v }

pattern ExprInt :: Int -> Expr
pattern ExprInt val = Expr { exprType = VarTypeInt, exprImpl = ExprIntImpl val }

pattern ExprFloat :: Double -> Expr
pattern ExprFloat val = Expr { exprType = VarTypeFloat, exprImpl = ExprFloatImpl val }

pattern ExprString :: String -> Expr
pattern ExprString val = Expr { exprType = VarTypeString, exprImpl = ExprStringImpl val }

pattern ExprBinOp :: BinOp -> Expr -> Expr -> Expr
pattern ExprBinOp op lhs rhs <- Expr { exprImpl = ExprBinOpImpl op lhs rhs } where
  ExprBinOp op lhs rhs = Expr { exprType = t, exprImpl = ExprBinOpImpl op lhs rhs }
    where
      t
       | exprType lhs == exprType rhs = exprType lhs
       | otherwise = error "Type mismatch"

pattern ExprUnOp :: UnOp -> Expr -> Expr
pattern ExprUnOp op e <- Expr { exprImpl = ExprUnOpImpl op e } where
  ExprUnOp op@UnIntToFloat e = Expr { exprType = t, exprImpl = ExprUnOpImpl op e }
    where
      t
       | exprType e == VarTypeInt = VarTypeFloat
       | otherwise = error "Type mismatch"
  ExprUnOp op e = Expr { exprType = t, exprImpl = ExprUnOpImpl op e }
    where
      t = exprType e
