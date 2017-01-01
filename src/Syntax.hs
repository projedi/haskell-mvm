module Syntax
  ( Program(..)
  , Symbol
  , VarID(..)
  , FunctionName(..)
  , VarType(..)
  , VarDecl(..)
  , Block(..)
  , Statement(..)
  , FunctionDecl(..)
  , FunctionDef(..)
  , FunctionCall(..)
  , Expr(..)
  ) where

import PreSyntax (VarType(..))

data Program = Program
  { programStatements :: Block
  , programLibraries :: [String]
  }

type Symbol = String

newtype VarID = VarID Int
  deriving (Eq, Ord, Show)

data FunctionName =
  FunctionName Symbol
  deriving (Eq, Ord)

data VarDecl =
  VarDecl VarType
          VarID

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
  | StatementAssign VarID
                    Expr
  | StatementIfElse Expr
                    Block
                    Block
  | StatementFunctionDef FunctionDef
  | StatementReturn (Maybe Expr)
  | StatementForeignFunctionDecl FunctionDecl

data FunctionDecl = FunctionDecl
  { funDeclRetType :: Maybe VarType
  , funDeclName :: FunctionName
  , funDeclParams :: [VarType]
  }

data FunctionDef = FunctionDef
  { funDefRetType :: Maybe VarType
  , funDefName :: FunctionName
  , funDefParams :: [VarDecl]
  , funDefBody :: Block
  }

data FunctionCall =
  FunctionCall FunctionName
               [Expr]

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
