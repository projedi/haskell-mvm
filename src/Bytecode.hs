{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bytecode
  ( Bytecode(..)
  , ConstID(..)
  , FunID(..)
  , ForeignFunctionDecl(..)
  , VarID(..)
  , LabelID(..)
  , BytecodeFunction(..)
  , Op(..)
  ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Monoid ((<>))

import Syntax
       (VarType, VarID(..), FunID(..), ForeignFunctionDecl(..))
import Value (Value)

newtype ConstID =
  ConstID Int
  deriving (Show)

data Bytecode = Bytecode
  { bytecodeFunctions :: IntMap BytecodeFunction
  , bytecodeLibraries :: [String]
  , bytecodeConstants :: IntMap Value
  , bytecodeForeignFunctions :: IntMap ForeignFunctionDecl
  }

instance Monoid Bytecode where
  mempty =
    Bytecode
    { bytecodeFunctions = IntMap.empty
    , bytecodeLibraries = []
    , bytecodeConstants = IntMap.empty
    , bytecodeForeignFunctions = IntMap.empty
    }
  lhs `mappend` rhs =
    Bytecode
    { bytecodeFunctions =
      IntMap.unionWith (<>) (bytecodeFunctions lhs) (bytecodeFunctions rhs)
    , bytecodeLibraries = bytecodeLibraries lhs ++ bytecodeLibraries rhs
    , bytecodeConstants =
      IntMap.union (bytecodeConstants lhs) (bytecodeConstants rhs)
    , bytecodeForeignFunctions =
      IntMap.union (bytecodeForeignFunctions lhs) (bytecodeForeignFunctions rhs)
    }

newtype BytecodeFunction =
  BytecodeFunction [Op]
  deriving (Monoid, Show)

newtype LabelID =
  LabelID Int
  deriving (Show)

-- Args are in pop order. Return value is on top of the stack.
data Op
  = OpCall FunID
  | OpIntroVar VarID
               VarType
  | OpReturn
  | OpForeignCall FunID
  | OpLabel LabelID
  | OpJump LabelID
  | OpJumpIfZero LabelID
  | OpPushInt ConstID
  | OpPushFloat ConstID
  | OpPushString ConstID
  | OpPop
  | OpStore VarID -- ^ Store top of the stack in the variable.
  | OpLoad VarID -- ^ Load variable onto the stack.
  | OpNegateInt
  | OpNegateFloat
  | OpPlusInt
  | OpPlusFloat
  | OpMinusInt
  | OpMinusFloat
  | OpTimesInt
  | OpTimesFloat
  | OpDivInt
  | OpDivFloat
  | OpModInt
  | OpBitAndInt
  | OpBitOrInt
  | OpBitXorInt
  | OpNotInt
  | OpAndInt
  | OpOrInt
  | OpEqInt
  | OpEqFloat
  | OpLtInt
  | OpLtFloat
  | OpIntToFloat

instance Show Op where
  show (OpCall (FunID f)) = "call " ++ show f
  show (OpIntroVar v t) = "var " ++ show v ++ " : " ++ show t
  show OpReturn = "ret"
  show (OpForeignCall (FunID f)) = "foreign " ++ show f
  show (OpLabel (LabelID l)) = "lbl " ++ show l
  show (OpJump (LabelID l)) = "jmp " ++ show l
  show (OpJumpIfZero (LabelID l)) = "jz " ++ show l
  show (OpPushInt i) = "ipush " ++ show i
  show (OpPushFloat f) = "fpush " ++ show f
  show (OpPushString s) = "spush " ++ show s
  show OpPop = "pop"
  show (OpStore v) = "store " ++ show v
  show (OpLoad v) = "load " ++ show v
  show OpNegateInt = "ineg"
  show OpNegateFloat = "fneg"
  show OpPlusInt = "iplus"
  show OpPlusFloat = "fplus"
  show OpMinusInt = "iminus"
  show OpMinusFloat = "fminus"
  show OpTimesInt = "imul"
  show OpTimesFloat = "fmul"
  show OpDivInt = "idiv"
  show OpDivFloat = "fdiv"
  show OpModInt = "mod"
  show OpBitAndInt = "band"
  show OpBitOrInt = "bor"
  show OpBitXorInt = "bxor"
  show OpNotInt = "not"
  show OpAndInt = "and"
  show OpOrInt = "or"
  show OpEqInt = "ieq"
  show OpEqFloat = "feq"
  show OpLtInt = "ilt"
  show OpLtFloat = "flt"
  show OpIntToFloat = "i2f"
