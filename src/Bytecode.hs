{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Bytecode where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Monoid ((<>))

import qualified Syntax

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
  | OpIntroVar VarID
               Syntax.VarType
  | OpReturn
    -- TODO: That's a very long op.
  | OpForeignCall String
                  (Maybe Syntax.VarType)
                  [Syntax.VarType]
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
  | OpIntToString
  | OpFloatToString

instance Show Op where
  show (OpCall (FunID f)) = "call " ++ show f
  show (OpIntroVar (VarID v) t) = "var " ++ show v ++ " : " ++ show t
  show OpReturn = "ret"
  show (OpForeignCall name rettype types) =
    "foreign " ++ name ++ " : " ++ show rettype ++ " " ++ show types
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
  show OpIntToString = "i2s"
  show OpFloatToString = "f2s"
