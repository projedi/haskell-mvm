{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, TemplateHaskell #-}

module Util
  ( doubleToString
  ) where

import Foreign.C.String (CString)
import qualified Foreign.C.String as CString
import Foreign.Marshal.Alloc (allocaBytes)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU

C.include "<stdio.h>"

doubleToString :: Double -> IO String
doubleToString d = do
  let buflength = 50
  allocaBytes buflength $
    \(bufptr :: CString) -> do
      let cbuflength = fromIntegral buflength
          cd = C.CDouble d
      [CU.block| void { snprintf($(char* bufptr), $(int cbuflength), "%g", $(double cd)); } |]
      CString.peekCString bufptr
