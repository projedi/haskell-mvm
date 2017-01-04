{-# LANGUAGE PatternSynonyms #-}
module Syntax
  ( Program(..)
  , VarID(..)
  , FunID(..)
  , VarType(..)
  , VarDecl(..)
  , Block(..)
  , Statement(..)
  , FunctionDef(..)
  , ForeignFunctionDecl(..)
  , FunctionCall(..)
  , BinOp(..)
  , binOpTypeFromArgs
  , UnOp(..)
  , unOpTypeFromArg
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
        ForeignFunctionDecl(..))

data Program = Program
  { programFunctions :: IntMap FunctionDef
  , programLibraries :: [String]
  , programForeignFunctions :: IntMap ForeignFunctionDecl
  , programLastFunID :: FunID
  , programLastVarID :: VarID
  }

data Block = Block
  { blockStatements :: [Statement]
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
  , funDefLocals :: [VarDecl]
  , funDefCaptures :: [VarID] -- Filled by SyntaxTrimmer
  , funDefBody :: Block
  }

data FunctionCall
  = NativeFunctionCall
    { nativeFunCallName :: FunID
    , nativeFunCallRetType :: Maybe VarType
    , nativeFunCallCaptures :: [VarID] -- Filled by SyntaxTrimmer
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

binOpTypeFromArgs :: BinOp -> VarType -> VarType -> VarType
binOpTypeFromArgs BinPlus VarTypeInt VarTypeInt = VarTypeInt
binOpTypeFromArgs BinPlus VarTypeFloat VarTypeFloat = VarTypeFloat
binOpTypeFromArgs BinPlus _ _ = error "Type mismatch"
binOpTypeFromArgs BinMinus VarTypeInt VarTypeInt = VarTypeInt
binOpTypeFromArgs BinMinus VarTypeFloat VarTypeFloat = VarTypeFloat
binOpTypeFromArgs BinMinus _ _ = error "Type mismatch"
binOpTypeFromArgs BinTimes VarTypeInt VarTypeInt = VarTypeInt
binOpTypeFromArgs BinTimes VarTypeFloat VarTypeFloat = VarTypeFloat
binOpTypeFromArgs BinTimes _ _ = error "Type mismatch"
binOpTypeFromArgs BinDiv VarTypeInt VarTypeInt = VarTypeInt
binOpTypeFromArgs BinDiv VarTypeFloat VarTypeFloat = VarTypeFloat
binOpTypeFromArgs BinDiv _ _ = error "Type mismatch"
binOpTypeFromArgs BinMod VarTypeInt VarTypeInt = VarTypeInt
binOpTypeFromArgs BinMod _ _ = error "Type mismatch"
binOpTypeFromArgs BinBitAnd VarTypeInt VarTypeInt = VarTypeInt
binOpTypeFromArgs BinBitAnd _ _ = error "Type mismatch"
binOpTypeFromArgs BinBitOr VarTypeInt VarTypeInt = VarTypeInt
binOpTypeFromArgs BinBitOr _ _ = error "Type mismatch"
binOpTypeFromArgs BinBitXor VarTypeInt VarTypeInt = VarTypeInt
binOpTypeFromArgs BinBitXor _ _ = error "Type mismatch"
binOpTypeFromArgs BinAnd VarTypeInt VarTypeInt = VarTypeInt
binOpTypeFromArgs BinAnd _ _ = error "Type mismatch"
binOpTypeFromArgs BinOr VarTypeInt VarTypeInt = VarTypeInt
binOpTypeFromArgs BinOr _ _ = error "Type mismatch"
binOpTypeFromArgs BinEq VarTypeInt VarTypeInt = VarTypeInt
binOpTypeFromArgs BinEq VarTypeFloat VarTypeFloat = VarTypeInt
binOpTypeFromArgs BinEq _ _ = error "Type mismatch"
binOpTypeFromArgs BinLt VarTypeInt VarTypeInt = VarTypeInt
binOpTypeFromArgs BinLt VarTypeFloat VarTypeFloat = VarTypeInt
binOpTypeFromArgs BinLt _ _ = error "Type mismatch"

data UnOp
  = UnNeg
  | UnNot
  | UnIntToFloat

unOpTypeFromArg :: UnOp -> VarType -> VarType
unOpTypeFromArg UnNeg VarTypeInt = VarTypeInt
unOpTypeFromArg UnNeg VarTypeFloat = VarTypeFloat
unOpTypeFromArg UnNeg _ = error "Type mismatch"
unOpTypeFromArg UnNot VarTypeInt = VarTypeInt
unOpTypeFromArg UnNot _ = error "Type mismatch"
unOpTypeFromArg UnIntToFloat VarTypeInt = VarTypeFloat
unOpTypeFromArg UnIntToFloat _ = error "Type mismatch"

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
      t = binOpTypeFromArgs op (exprType lhs) (exprType rhs)

pattern ExprUnOp :: UnOp -> Expr -> Expr
pattern ExprUnOp op e <- Expr { exprImpl = ExprUnOpImpl op e } where
  ExprUnOp op e = Expr { exprType = t, exprImpl = ExprUnOpImpl op e }
    where
      t = unOpTypeFromArg op $ exprType e
