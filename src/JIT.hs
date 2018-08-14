{-# LANGUAGE ForeignFunctionInterface #-}

module JIT
  ( jit
  ) where

import Foreign.Ptr (FunPtr, Ptr, castPtrToFunPtr)

import BinarySyntax

jit :: Program -> IO ()
jit p = run $ programCode p

foreign import ccall "dynamic" mkRun :: FunPtr (IO ()) -> IO ()

run :: Ptr () -> IO ()
run p = mkRun (castPtrToFunPtr p)
