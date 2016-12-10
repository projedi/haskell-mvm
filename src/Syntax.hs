module Syntax where

type Symbol = String

data VarName = VarName Symbol
  deriving (Eq, Show)

data FunctionName = FunctionName Symbol
  deriving (Eq, Show)

data VarType
  = VarTypeInt
  | VarTypeFloat
  | VarTypeString
  deriving (Eq, Show)

data VarDef = VarDef VarType VarName
  deriving (Eq, Show)

data Statement
  = StatementBlock [Statement]
  | StatementFunctionCall FunctionCall
  | StatementWhile Expr Statement
  | StatementVarDecl VarDef
  | StatementFunctionDecl FunctionDecl
  | StatementAssign VarName Expr
  | StatementIfElse Expr Statement Statement
  | StatementIf Expr Statement
  | StatementFor VarName Expr Expr Statement
  | StatementFunctionDef FunctionDecl [Statement]
  | StatementReturn (Maybe Expr)
  deriving (Eq, Show)

data FunctionDecl
  = FunctionDecl (Maybe VarType) FunctionName [VarDef]
  deriving (Eq, Show)

data FunctionCall
  = FunctionCall FunctionName [Expr]
  deriving (Eq, Show)

data Expr
  = ExprFunctionCall FunctionCall
  | ExprVar VarName
  | ExprInt Int
  | ExprFloat Double
  | ExprString String
  | ExprNeg Expr
  | ExprPlus Expr Expr
  | ExprMinus Expr Expr
  | ExprTimes Expr Expr
  | ExprDiv Expr Expr
  | ExprMod Expr Expr
  | ExprEq Expr Expr
  | ExprNeq Expr Expr
  | ExprLt Expr Expr
  | ExprLeq Expr Expr
  | ExprGt Expr Expr
  | ExprGeq Expr Expr
  deriving (Eq, Show)
