module PreSyntax where

import Data.Int (Int64)

data Program = Program
  { programStatements :: [Statement]
  , programLibraries :: [String]
  } deriving (Show)

type Symbol = String

newtype VarName =
  VarName Symbol
  deriving (Eq, Ord, Show)

newtype FunctionName =
  FunctionName Symbol
  deriving (Eq, Ord, Show)

data VarType
  = VarTypeInt
  | VarTypeFloat
  | VarTypeString
  | VarTypePtr VarType
  deriving (Eq, Ord)

instance Show VarType where
  show VarTypeInt = "int"
  show VarTypeFloat = "double"
  show VarTypeString = "string"
  show (VarTypePtr t) = show t ++ "*"

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
  | StatementVarDef VarDecl
                    Expr
  | StatementFunctionDecl FunctionDecl
  | StatementAssign VarName
                    Expr
  | StatementAssignPlus VarName
                        Expr
  | StatementAssignMinus VarName
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
  | ExprInt Int64
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
