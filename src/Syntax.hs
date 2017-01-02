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

data Expr
  = ExprFunctionCall FunctionCall
  | ExprVar VarID
  | ExprInt Int
  | ExprFloat Double
  | ExprString String
  | ExprNeg Expr
  | ExprPlus Expr
             Expr
  | ExprMinus Expr
              Expr
  | ExprTimes Expr
              Expr
  | ExprDiv Expr
            Expr
  | ExprMod Expr
            Expr
  | ExprBitAnd Expr
               Expr
  | ExprBitOr Expr
              Expr
  | ExprBitXor Expr
               Expr
  | ExprNot Expr
  | ExprAnd Expr
            Expr
  | ExprOr Expr
           Expr
  | ExprEq Expr
           Expr
  | ExprLt Expr
           Expr
