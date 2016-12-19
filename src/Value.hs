module Value
  ( Value(..)
  , fromBool
  , toBool
  , typeIs
  , typeof
  , defaultValueFromType
  , convert
  ) where

import Syntax (VarType(..))
import Util

data Value
  = ValueInt Int
  | ValueFloat Double
  | ValueString String

instance Eq Value where
  (ValueInt il) == (ValueInt ir) = il == ir
  (ValueFloat fl) == (ValueFloat fr) = fl == fr
  (ValueString sl) == (ValueString sr) = sl == sr
  _ == _ = error "Type mismatch"

instance Ord Value where
  compare (ValueInt il) (ValueInt ir) = compare il ir
  compare (ValueFloat fl) (ValueFloat fr) = compare fl fr
  compare (ValueString sl) (ValueString sr) = compare sl sr
  compare _ _ = error "Type mismatch"

instance Num Value where
  (ValueInt il) + (ValueInt ir) = ValueInt (il + ir)
  (ValueInt il) + (ValueFloat fr) = ValueFloat (fromIntegral il + fr)
  (ValueFloat fl) + (ValueInt ir) = ValueFloat (fl + fromIntegral ir)
  (ValueFloat fl) + (ValueFloat fr) = ValueFloat (fl + fr)
  (ValueString sl) + (ValueString sr) = ValueString (sl ++ sr)
  _ + _ = error "Type mismatch"
  (ValueInt il) * (ValueInt ir) = ValueInt (il * ir)
  (ValueInt il) * (ValueFloat fr) = ValueFloat (fromIntegral il * fr)
  (ValueFloat fl) * (ValueInt ir) = ValueFloat (fl * fromIntegral ir)
  (ValueFloat fl) * (ValueFloat fr) = ValueFloat (fl * fr)
  _ * _ = error "Type mismatch"
  fromInteger i = ValueInt (fromInteger i)
  signum (ValueInt i) = ValueInt (signum i)
  signum (ValueFloat f) = ValueFloat (signum f)
  signum _ = error "Type mismatch"
  abs (ValueInt i) = ValueInt (abs i)
  abs (ValueFloat f) = ValueFloat (abs f)
  abs _ = error "Type mismatch"
  negate (ValueInt i) = ValueInt (negate i)
  negate (ValueFloat f) = ValueFloat (negate f)
  negate _ = error "Type mismatch"

instance Enum Value where
  toEnum i = ValueInt (toEnum i)
  fromEnum (ValueInt i) = fromEnum i
  fromEnum _ = error "Type mismatch"

instance Real Value where
  toRational (ValueInt i) = toRational i
  toRational (ValueFloat f) = toRational f
  toRational _ = error "Type mismatch"

instance Integral Value where
  toInteger (ValueInt i) = toInteger i
  toInteger _ = error "Type mismatch"
  quotRem (ValueInt il) (ValueInt ir) =
    let (rl, rr) = quotRem il ir
    in (ValueInt rl, ValueInt rr)
  quotRem _ _ = error "Type mismatch"

instance Fractional Value where
  fromRational r = ValueFloat (fromRational r)
  (ValueInt il) / (ValueInt ir) = ValueInt (il `quot` ir)
  (ValueFloat fl) / (ValueInt ir) = ValueFloat (fl / fromIntegral ir)
  (ValueInt il) / (ValueFloat fr) = ValueFloat (fromIntegral il / fr)
  (ValueFloat fl) / (ValueFloat fr) = ValueFloat (fl / fr)
  _ / _ = error "Type mismatch"

toBool :: Value -> Bool
toBool (ValueInt i) = i /= 0
toBool _ = error "Type mismatch"

fromBool :: Bool -> Value
fromBool True = ValueInt 1
fromBool False = ValueInt 0

instance Show Value where
  show (ValueInt i) = show i
  show (ValueFloat f) = doubleToString f
  show (ValueString s) = s

typeIs :: Value -> VarType -> Bool
typeIs v vtype = typeof v == vtype

typeof :: Value -> VarType
typeof (ValueInt _) = VarTypeInt
typeof (ValueFloat _) = VarTypeFloat
typeof (ValueString _) = VarTypeString

defaultValueFromType :: VarType -> Value
defaultValueFromType VarTypeInt = ValueInt 0
defaultValueFromType VarTypeFloat = ValueFloat 0
defaultValueFromType VarTypeString = ValueString ""

convert :: Value -> VarType -> Value
convert v vtype
  | v `typeIs` vtype = v
convert (ValueInt i) VarTypeFloat = ValueFloat $ fromIntegral i
convert _ _ = error "Type mismatch"
