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
  , BinOp(..)
  , UnOp(..)
  , Expr(..)
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
  = NativeFunctionCall FunID
                       [Expr]
  | ForeignFunctionCall FunID
                        [Expr]
  | PrintCall [Expr]

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

data Expr
  = ExprFunctionCall FunctionCall
  | ExprVar VarID
  | ExprInt Int
  | ExprFloat Double
  | ExprString String
  | ExprBinOp BinOp
              Expr
              Expr
  | ExprUnOp UnOp
             Expr
