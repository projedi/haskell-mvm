{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bytecode where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Monoid ((<>))

newtype Bytecode =
  Bytecode (IntMap BytecodeFunction)
  deriving (Show)

instance Monoid Bytecode where
  mempty = Bytecode IntMap.empty
  (Bytecode lhs) `mappend` (Bytecode rhs) =
    Bytecode (IntMap.unionWith (<>) lhs rhs)

newtype BytecodeFunction =
  BytecodeFunction [Op]
  deriving (Monoid, Show)

newtype FunID =
  FunID Int
  deriving (Show)

newtype LabelID =
  LabelID Int
  deriving (Show)

newtype VarID =
  VarID Int
  deriving (Show)

-- Args are in pop order. Return value is on top of the stack.
data Op
  = OpCall FunID
  | OpReturn
  | OpReturnWithValue
  | OpForeignCall String
  | OpPrintCall -- ^ count of args, string args.
  | OpDlopenCall -- ^ count of args, string args.
  | OpLabel LabelID
  | OpJump LabelID
  | OpJumpIfZero LabelID
  | OpPushInt Int
  | OpPushFloat Double
  | OpPushString String
  | OpPop
  | OpStore VarID
  | OpLoad VarID
  | OpNegateInt
  | OpNegateFloat
  | OpPlusInt
  | OpPlusFloat
  | OpPlusString
  | OpMinusInt
  | OpMinusFloat
  | OpTimesInt
  | OpTimesFloat
  | OpDivInt
  | OpDivFloat
  | OpModInt
  | OpNotInt
  | OpEqInt
  | OpEqFloat
  | OpEqString
  | OpLtInt
  | OpLtFloat
  | OpLtString
  | OpIntToFloat
  | OpIntToString
  | OpFloatToString

instance Show Op where
  show (OpCall (FunID f)) = "call " ++ show f
  show OpReturn = "ret"
  show OpReturnWithValue = "retv"
  show (OpForeignCall name) = "fcall " ++ name
  show OpPrintCall = "print"
  show OpDlopenCall = "dlopen"
  show (OpLabel (LabelID l)) = "lbl " ++ show l
  show (OpJump (LabelID l)) = "jmp " ++ show l
  show (OpJumpIfZero (LabelID l)) = "jz " ++ show l
  show (OpPushInt i) = "ipush " ++ show i
  show (OpPushFloat f) = "fpush " ++ show f
  show (OpPushString s) = "spush " ++ show s
  show OpPop = "pop"
  show (OpStore (VarID v)) = "store " ++ show v
  show (OpLoad (VarID v)) = "load " ++ show v
  show OpNegateInt = "ineg"
  show OpNegateFloat = "fneg"
  show OpPlusInt = "iplus"
  show OpPlusFloat = "fplus"
  show OpPlusString = "splus"
  show OpMinusInt = "iminus"
  show OpMinusFloat = "fminus"
  show OpTimesInt = "imul"
  show OpTimesFloat = "fmul"
  show OpDivInt = "idiv"
  show OpDivFloat = "fdiv"
  show OpModInt = "mod"
  show OpNotInt = "not"
  show OpEqInt = "ieq"
  show OpEqFloat = "feq"
  show OpEqString = "seq"
  show OpLtInt = "ilt"
  show OpLtFloat = "flt"
  show OpLtString = "slt"
  show OpIntToFloat = "i2f"
  show OpIntToString = "i2s"
  show OpFloatToString = "f2s"
