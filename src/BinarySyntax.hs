module BinarySyntax
  ( Program(..)
  ) where

import Data.Int (Int64)
import Data.IntMap (IntMap)
import Foreign.Ptr (Ptr)

data Program = Program
  { programCode :: Ptr ()
  , programSize :: Int64
  , programForeignFunctions :: IntMap Int64
  , programStrings :: IntMap Int64
  , programLabels :: IntMap Int64
  }
