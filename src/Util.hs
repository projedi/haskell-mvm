{-# LANGUAGE QuasiQuotes, ScopedTypeVariables, TemplateHaskell,
  FlexibleContexts #-}

module Util
  ( doubleToString
  , doubleToWord64
  , allocateExecutablePage
  , freeExecutablePage
  ) where

import Data.Array.ST (MArray, STUArray, newArray, readArray)
import Data.Array.Unsafe (castSTUArray)
import Data.Int (Int64)
import Data.Word (Word64)
import Foreign.C.String (CString)
import qualified Foreign.C.String as CString
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr)
import GHC.ST (ST, runST)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU

C.include "<stdio.h>"

C.include "<sys/mman.h>"

C.include "<unistd.h>"

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

-- Peeked at asmjit.
allocateExecutablePage :: Int64 -> IO (Ptr a, Int64)
allocateExecutablePage len = do
  let len' = fromIntegral len
  alignedSize <-
    [C.block| size_t {
                    const size_t alignment = getpagesize();
                    const size_t size = $(size_t len');
                    return (size + alignment) & ~(alignment - 1);
                  }|]
  ptr <-
    [C.block| void* {
            const int protection = PROT_READ | PROT_WRITE | PROT_EXEC;
            return mmap(NULL, $(size_t alignedSize), protection, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
          }|]
  pure (castPtr ptr, fromIntegral alignedSize)

freeExecutablePage :: Ptr a -> Int64 -> IO ()
freeExecutablePage p len = do
  let len' = fromIntegral len
  let p' = castPtr p
  [C.block| void { munmap($(void* p'), $(size_t len')); } |]
