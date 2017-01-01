module Syntax
  ( Program(..)
  , Symbol
  , VarName(..)
  , FunctionName(..)
  , VarType(..)
  , VarDecl(..)
  , Statement(..)
  , FunctionDecl(..)
  , FunctionCall(..)
  , Expr(..)
  ) where

import PreSyntax (VarType(..))

data Program = Program
  { programStatements :: [Statement]
  , programLibraries :: [String]
  } deriving (Show)

type Symbol = String

data VarName =
  VarName Symbol
  deriving (Eq, Ord, Show)

data FunctionName =
  FunctionName Symbol
  deriving (Eq, Ord, Show)

data VarDecl =
  VarDecl VarType
          VarName
  deriving (Eq, Show)

data Statement
  = StatementNoop
  | StatementBlock [Statement]
  | StatementFunctionCall FunctionCall
  | StatementWhile Expr
                   Statement
  | StatementVarDecl VarDecl
  | StatementFunctionDecl FunctionDecl
  | StatementAssign VarName
                    Expr
  | StatementIfElse Expr
                    Statement
                    Statement
  | StatementFor VarName
                 Expr
                 Expr
                 Statement
  | StatementFunctionDef FunctionDecl
                         [Statement]
  | StatementReturn (Maybe Expr)
  | StatementForeignFunctionDecl FunctionDecl
  deriving (Eq, Show)

data FunctionDecl =
  FunctionDecl (Maybe VarType)
               FunctionName
               [VarDecl]
  deriving (Eq, Show)

data FunctionCall =
  FunctionCall FunctionName
               [Expr]
  deriving (Eq, Show)

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
  deriving (Eq, Show)
