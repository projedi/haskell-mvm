module Syntax
  ( Program(..)
  , Symbol
  , VarName(..)
  , FunctionName(..)
  , VarType(..)
  , VarDecl(..)
  , Block(..)
  , Statement(..)
  , FunctionDecl(..)
  , FunctionCall(..)
  , Expr(..)
  ) where

import PreSyntax (VarType(..))

data Program = Program
  { programStatements :: Block
  , programLibraries :: [String]
  }

type Symbol = String

data VarName =
  VarName Symbol
  deriving (Eq, Ord)

data FunctionName =
  FunctionName Symbol
  deriving (Eq, Ord)

data VarDecl =
  VarDecl VarType
          VarName

data Block = Block
  { blockVariables :: [VarDecl]
  , blockStatements :: [Statement]
  }

data Statement
  = StatementNoop
  | StatementBlock Block
  | StatementFunctionCall FunctionCall
  | StatementWhile Expr
                   Block
  | StatementFunctionDecl FunctionDecl
  | StatementAssign VarName
                    Expr
  | StatementIfElse Expr
                    Block
                    Block
  | StatementFor VarName
                 Expr
                 Expr
                 Block
  | StatementFunctionDef FunctionDecl
                         Block
  | StatementReturn (Maybe Expr)
  | StatementForeignFunctionDecl FunctionDecl

data FunctionDecl =
  FunctionDecl (Maybe VarType)
               FunctionName
               [VarDecl]

data FunctionCall =
  FunctionCall FunctionName
               [Expr]

data Expr
  = ExprFunctionCall FunctionCall
  | ExprVar VarName
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
