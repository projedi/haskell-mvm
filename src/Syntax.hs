module Syntax
  ( Program(..)
  , Symbol
  , VarID(..)
  , FunID(..)
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

newtype FunID = FunID Int
  deriving (Eq, Ord, Show)

data VarDecl =
  VarDecl VarType
          VarID

data Block = Block
  { blockVariables :: [VarDecl]
  , blockFunctions :: [FunctionDef]
  , blockForeignFunctions :: [(FunctionDecl, String)]
  , blockStatements :: [Statement]
  }

data Statement
  = StatementNoop
  | StatementBlock Block
  | StatementFunctionCall FunctionCall
  | StatementWhile Expr
                   Block
  | StatementAssign VarID
                    Expr
  | StatementIfElse Expr
                    Block
                    Block
  | StatementReturn (Maybe Expr)

data FunctionDecl = FunctionDecl
  { funDeclRetType :: Maybe VarType
  , funDeclName :: FunID
  , funDeclParams :: [VarType]
  }

data FunctionDef = FunctionDef
  { funDefRetType :: Maybe VarType
  , funDefName :: FunID
  , funDefParams :: [VarDecl]
  , funDefBody :: Block
  }

data FunctionCall
  = NativeFunctionCall FunID [Expr]
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
