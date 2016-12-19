module Syntax where

data Program =
  Program [Statement]
  deriving (Show)

type Symbol = String

data VarName =
  VarName Symbol
  deriving (Eq, Ord, Show)

data FunctionName =
  FunctionName Symbol
  deriving (Eq, Ord, Show)

data VarType
  = VarTypeInt
  | VarTypeFloat
  | VarTypeString
  deriving (Eq, Ord)

instance Show VarType where
  show VarTypeInt = "int"
  show VarTypeFloat = "double"
  show VarTypeString = "string"

data VarDecl =
  VarDecl VarType
          VarName
  deriving (Eq, Show)

data Statement
  = StatementBlock [Statement]
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
  | StatementIf Expr
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
  | ExprNot Expr
  | ExprEq Expr
           Expr
  | ExprNeq Expr
            Expr
  | ExprLt Expr
           Expr
  | ExprLeq Expr
            Expr
  | ExprGt Expr
           Expr
  | ExprGeq Expr
            Expr
  deriving (Eq, Show)
