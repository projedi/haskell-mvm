module ResolvedSyntax
  ( Program(..)
  , VarID(..)
  , FunID(..)
  , ConstID(..)
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
import Value (Value)

data Program = Program
  { programFunctions :: IntMap FunctionDef
  , programLibraries :: [String]
  , programForeignFunctions :: IntMap ForeignFunctionDecl
  , programConstants :: IntMap Value
  }

newtype VarID =
  VarID Int
  deriving (Eq)

instance Show VarID where
  show (VarID i) = "var_" ++ show i

newtype FunID =
  FunID Int

instance Show FunID where
  show (FunID i) = "fun_" ++ show i

newtype ConstID =
  ConstID Int

instance Show ConstID where
  show (ConstID i) = "const_" ++ show i

data VarDecl =
  VarDecl VarType
          VarID

newtype Block = Block
  { blockStatements :: [Statement]
  }

data Statement
  = StatementBlock Block
  | StatementFunctionCall FunctionCall
  | StatementWhile Expr
                   Block
  | StatementAssign VarID
                    Expr
  | StatementAssignPlus VarID
                        Expr
  | StatementAssignMinus VarID
                         Expr
  | StatementIfElse Expr
                    Block
                    Block
  | StatementIf Expr
                Block
  | StatementReturn (Maybe Expr)

data FunctionDef = FunctionDef
  { funDefRetType :: Maybe VarType
  , funDefName :: FunID
  , funDefParams :: [VarDecl]
  , funDefLocals :: [VarDecl]
  , funDefCaptures :: [VarID]
  , funDefBody :: Block
  }

data ForeignFunctionDecl = ForeignFunctionDecl
  { foreignFunDeclRetType :: Maybe VarType
  , foreignFunDeclName :: FunID
  , foreignFunDeclRealName :: String
  , foreignFunDeclParams :: [VarType]
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
  | ExprConst ConstID
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
