module Syntax
  ( Program(..)
  , Symbol
  , VarID(..)
  , FunID(..)
  , VarType(..)
  , VarDecl(..)
  , Block(..)
  , Statement(..)
  , FunctionDef(..)
  , ForeignFunctionDecl(..)
  , FunctionCall(..)
  , Expr(..)
  ) where

import Data.IntMap (IntMap)

import PreSyntax (VarType(..))

data Program = Program
  { programStatements :: Block
  , programLibraries :: [String]
  , programForeignFunctions :: IntMap ForeignFunctionDecl
  }

type Symbol = String

newtype VarID = VarID Int

instance Show VarID where
  show (VarID i) = "var_" ++ show i

newtype FunID = FunID Int

instance Show FunID where
  show (FunID i) = "fun_" ++ show i

data VarDecl =
  VarDecl VarType
          VarID

data Block = Block
  { blockVariables :: [VarDecl]
  , blockFunctions :: [FunctionDef]
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

data FunctionDef = FunctionDef
  { funDefRetType :: Maybe VarType
  , funDefName :: FunID
  , funDefParams :: [VarDecl]
  , funDefBody :: Block
  }

data ForeignFunctionDecl = ForeignFunctionDecl
  { foreignFunDeclRetType :: Maybe VarType
  , foreignFunDeclName :: FunID
  , foreignFunDeclRealName :: String
  , foreignFunDeclParams :: [VarType]
  }

data FunctionCall
  = NativeFunctionCall FunID [Expr]
  | ForeignFunctionCall FunID [Expr]
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
