{-# LANGUAGE FlexibleContexts, ForeignFunctionInterface #-}

module JIT
  ( jit
  ) where

import Control.Monad
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Writer (MonadWriter, Writer, execWriter)
import qualified Control.Monad.Writer as Writer
import qualified Data.ByteString.Builder as BSBuilder
import qualified Data.ByteString.Builder.Extra as BSBuilder
import Data.Int (Int64)
import Data.IntMap (IntMap)
import Data.Word (Word32, Word8)
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
       (mapM translateInstruction is)
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

data REX = REX
  { rex_W :: Bool
  , rex_R :: Bool
  , rex_X :: Bool
  , rex_B :: Bool
  }

instance Monoid REX where
  mempty = REX {rex_W = False, rex_R = False, rex_X = False, rex_B = False}
  r1 `mappend` r2 =
    REX
      { rex_W = rex_W r1 || rex_W r2
      , rex_R = rex_R r1 || rex_R r2
      , rex_X = rex_X r1 || rex_X r2
      , rex_B = rex_B r1 || rex_B r2
      }

rexW :: REX
rexW = mempty {rex_W = True}

rexByte :: REX -> Word8
rexByte REX {rex_W = w, rex_R = r, rex_X = x, rex_B = b} =
  0x40 +
  (if b
     then 0x1
     else 0x0) +
  (if x
     then 0x2
     else 0x0) +
  (if r
     then 0x4
     else 0x0) +
  (if w
     then 0x8
     else 0x0)

-- True if needs extension
reg :: Register -> (Bool, Word8)
reg RegisterRAX = (False, 0)
reg RegisterRCX = (False, 1)
reg RegisterRDX = (False, 2)
reg RegisterRBX = (False, 3)
reg RegisterRSP = (False, 4)
reg RegisterRBP = (False, 5)
reg RegisterRSI = (False, 6)
reg RegisterRDI = (False, 7)
reg RegisterR8 = (True, 0)
reg RegisterR9 = (True, 1)

modRM :: Register -> IntOperand -> (REX, Word8, Maybe Word8, Maybe Word32)
modRM r (IntOperandRegister _ rm) =
  let (rExt, rBits) = reg r
      (rmExt, rmBits) = reg rm
   in ( mempty {rex_R = rExt, rex_B = rmExt}
      , 3 * 64 + rBits * 8 + rmBits
      , Nothing
      , Nothing)
modRM r (IntOperandPointer p) =
  let (rExt, rBits) = reg r
      (mode, rmExt, rmBits, sib, extra) =
        go (pointerBase p) (pointerDisplacement p)
   in ( mempty {rex_R = rExt, rex_B = rmExt}
      , mode * 64 + rBits * 8 + rmBits
      , sib
      , extra)
  where
    go :: Register -> Int64 -> (Word8, Bool, Word8, Maybe Word8, Maybe Word32)
    go rm@RegisterRAX d = stdGo rm d
    go rm@RegisterRCX d = stdGo rm d
    go rm@RegisterRDX d = stdGo rm d
    go rm@RegisterRBX d = stdGo rm d
    go rm@RegisterRSP d =
      let (mode, rmExt, rmBits, _, extra) = stdGo rm d
       in (mode, rmExt, rmBits, Just 36, extra)
    go rm@RegisterRBP d =
      (2, fst (reg rm), snd (reg rm), Nothing, Just (fromIntegral d))
    go rm@RegisterRSI d = stdGo rm d
    go rm@RegisterRDI d = stdGo rm d
    go rm@RegisterR8 d = stdGo rm d
    go rm@RegisterR9 d = stdGo rm d
    stdGo ::
         Register -> Int64 -> (Word8, Bool, Word8, Maybe Word8, Maybe Word32)
    stdGo rm d =
      ( if d == 0
          then 0
          else 2
      , fst (reg rm)
      , snd (reg rm)
      , Nothing
      , if d == 0
          then Nothing
          else Just (fromIntegral d))

byte :: (MonadWriter BSBuilder m) => Word8 -> m ()
byte = Writer.tell . BSBuilder.word8

word32 :: (MonadWriter BSBuilder m) => Word32 -> m ()
word32 = Writer.tell . BSBuilder.word32BE

instructionWithModRM ::
     REX -> [Word8] -> Register -> IntOperand -> Translator ()
instructionWithModRM x i r rm = do
  let (x', modRMByte, sibByte, extra) = modRM r rm
      x'' = x `mappend` x'
  byte $ rexByte x''
  mapM_ byte i
  byte $ modRMByte
  maybe (pure ()) byte sibByte
  maybe (pure ()) word32 extra

translateInstruction :: Instruction -> Translator ()
translateInstruction (InstructionCMP r x) = instructionWithModRM rexW [0x3b] r x
