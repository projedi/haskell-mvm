{-# LANGUAGE ForeignFunctionInterface #-}

module JIT
  ( jit
  ) where

import Control.Monad
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Writer (Writer, execWriter)
import qualified Data.ByteString.Builder as BSBuilder
import qualified Data.ByteString.Builder.Extra as BSBuilder
import Data.Int (Int64)
import Data.IntMap (IntMap)
import Data.Word (Word8)
import qualified Foreign.C.String as Foreign
import Foreign.C.String (CString)
import qualified Foreign.Marshal.Alloc as Foreign
import Foreign.Ptr
  ( FunPtr
  , IntPtr(..)
  , Ptr
  , castFunPtrToPtr
  , castPtrToFunPtr
  , plusPtr
  , ptrToIntPtr
  )

import ASMSyntax
import ForeignEval

type BSBuilder = BSBuilder.Builder

jit :: Program -> IO ()
jit p = do
  libhandles <-
    forM (programLibraries p) $ \l -> do
      Right h <- dlopen l
      pure h
  ffuns <-
    forM (programForeignFunctions p) $ \fdecl -> do
      Just f <- findSymbolRaw $ foreignFunDeclRealName fdecl
      pure f
  strings <- forM (programStrings p) Foreign.newCString
  let (labels, programSize) = resolveLabelsAndProgramSize (programCode p)
  codeLocation <- Foreign.mallocBytes (fromIntegral programSize)
  let binaryCode =
        translateCode
          (programCode p)
          ffuns
          strings
          labels
          (ptrToInt64 codeLocation)
  writeBSBuilder binaryCode codeLocation programSize
  run codeLocation
  Foreign.free codeLocation
  forM_ strings Foreign.free
  forM_ libhandles dlclose

resolveLabelsAndProgramSize :: [Instruction] -> (IntMap Int64, Int64)
resolveLabelsAndProgramSize = _

ptrToInt64 :: Ptr a -> Int64
ptrToInt64 p =
  let (IntPtr v) = ptrToIntPtr p
   in fromIntegral v

translateCode ::
     [Instruction]
  -> IntMap (FunPtr ())
  -> IntMap CString
  -> IntMap Int64
  -> Int64
  -> BSBuilder
translateCode is ffs ss ls off =
  execWriter
    (runReaderT
       (translateCodeM is)
       ConstEnv
         { envForeignFuns = (ptrToInt64 . castFunPtrToPtr) <$> ffs
         , envStrings = ptrToInt64 <$> ss
         , envLabels = ls
         , envCodeOffset = off
         })

writeBSBuilder :: BSBuilder -> Ptr Word8 -> Int64 -> IO ()
writeBSBuilder bs p l = go (BSBuilder.runBuilder bs) p (fromIntegral l)
  where
    go :: BSBuilder.BufferWriter -> Ptr Word8 -> Int -> IO ()
    go w dest len = do
      (bytesWritten, next) <- w dest len
      let len' = len - bytesWritten
      case next of
        BSBuilder.Done
          | len' == 0 -> pure ()
          | otherwise -> error $ "Unconsumed " ++ show len' ++ " bytes"
        BSBuilder.More s nextW
          | len' >= s -> go nextW (dest `plusPtr` bytesWritten) len'
          | otherwise -> error $ "Not enough space in buffer: " ++ show len'
        BSBuilder.Chunk _ nextW
          | len' < 0 -> error $ "Not enough space in buffer" ++ show len'
          | otherwise -> go nextW (dest `plusPtr` bytesWritten) len'

foreign import ccall "dynamic" mkRun :: FunPtr (IO ()) -> IO ()

run :: Ptr Word8 -> IO ()
run p = mkRun (castPtrToFunPtr p)

data ConstEnv = ConstEnv
  { envForeignFuns :: IntMap Int64
  , envStrings :: IntMap Int64
  , envLabels :: IntMap Int64
  , envCodeOffset :: Int64
  }

type Translator = ReaderT ConstEnv (Writer BSBuilder)

translateCodeM :: [Instruction] -> Translator ()
translateCodeM = _
