{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, TemplateHaskell,
  FlexibleContexts #-}

module Util
  ( doubleToString
  , doubleToWord64
  ) where

import Data.Array.ST (MArray, STUArray, newArray, readArray)
import Data.Array.Unsafe (castSTUArray)
import Data.Word (Word64)
import Foreign.C.String (CString)
import qualified Foreign.C.String as CString
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.ST (ST, runST)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU

C.include "<stdio.h>"

doubleToString :: Double -> IO String
doubleToString d = do
  let buflength = 50
  allocaBytes buflength $ \(bufptr :: CString) -> do
    let cbuflength = fromIntegral buflength
        cd = C.CDouble d
    [CU.block| void { snprintf($(char* bufptr), $(int cbuflength), "%g", $(double cd)); } |]
    CString.peekCString bufptr

{-# INLINE cast #-}
cast ::
     (MArray (STUArray s) a (ST s), MArray (STUArray s) b (ST s)) => a -> ST s b
cast x = newArray (0 :: Int, 0) x >>= castSTUArray >>= flip readArray 0

doubleToWord64 :: Double -> Word64
doubleToWord64 x = runST (cast x)
