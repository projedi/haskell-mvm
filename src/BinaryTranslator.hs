{-# LANGUAGE FlexibleContexts #-}

module BinaryTranslator
  ( withBinary
  ) where

import Control.Monad
import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Reader as Reader
import Control.Monad.State (MonadState, StateT, evalStateT, execStateT)
import qualified Control.Monad.State as State
import Control.Monad.Writer (MonadWriter, Writer, execWriter, runWriter)
import qualified Control.Monad.Writer as Writer
import qualified Data.ByteString.Builder as BSBuilder
import qualified Data.ByteString.Builder.Extra as BSBuilder
import Data.Int (Int32, Int64)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Word (Word32, Word8)
import qualified Foreign.C.String as Foreign
import qualified Foreign.Marshal.Alloc as Foreign
import Foreign.Ptr
  ( IntPtr(..)
  , Ptr
  , castFunPtrToPtr
  , castPtr
  , plusPtr
  , ptrToIntPtr
  )

import ASMSyntax
import qualified BinarySyntax
import ForeignEval

type BSBuilder = BSBuilder.Builder

withBinary :: Program -> (BinarySyntax.Program -> IO a) -> IO a
withBinary p fun = do
  libhandles <-
    forM (programLibraries p) $ \l -> do
      Right h <- dlopen l
      pure h
  ffuns <-
    forM (programForeignFunctions p) $ \fdecl -> do
      Just f <- findSymbolRaw $ foreignFunDeclRealName fdecl
      pure f
  strings <- forM (programStrings p) Foreign.newCString
  let constEnv1 =
        ConstEnv
          { envForeignFuns = (ptrToInt64 . castFunPtrToPtr) <$> ffuns
          , envStrings = ptrToInt64 <$> strings
          , envLabels = IntMap.empty
          }
  let (labels, programSize) =
        resolveLabelsAndProgramSize (programCode p) constEnv1
  codeLocation <- Foreign.mallocBytes (fromIntegral programSize)
  let codeOffset = ptrToInt64 codeLocation
  let constEnv2 = constEnv1 {envLabels = (+ codeOffset) <$> labels}
  let binaryCode = translateCode (programCode p) codeOffset constEnv2
  writeBSBuilder binaryCode codeLocation programSize
  res <-
    fun $
    BinarySyntax.Program
      { BinarySyntax.programCode = castPtr codeLocation
      , BinarySyntax.programSize = programSize
      , BinarySyntax.programForeignFunctions = envForeignFuns constEnv2
      , BinarySyntax.programStrings = envStrings constEnv2
      , BinarySyntax.programLabels = envLabels constEnv2
      }
  Foreign.free codeLocation
  forM_ strings Foreign.free
  forM_ libhandles dlclose
  pure res

resolveLabelsAndProgramSize ::
     [Instruction] -> ConstEnv -> (IntMap Int64, Int64)
resolveLabelsAndProgramSize is constEnv =
  (envNewLables finalEnv, envCodeOffset finalEnv)
  where
    (finalEnv, _) =
      runWriter
        (runReaderT
           (execStateT
              (mapM translateInstruction is)
              Env {envCodeOffset = 0, envNewLables = IntMap.empty})
           constEnv)

ptrToInt64 :: Ptr a -> Int64
ptrToInt64 p =
  let (IntPtr v) = ptrToIntPtr p
   in fromIntegral v

translateCode :: [Instruction] -> Int64 -> ConstEnv -> BSBuilder
translateCode is off constEnv =
  execWriter
    (runReaderT
       (evalStateT
          (mapM translateInstruction is)
          Env {envCodeOffset = off, envNewLables = IntMap.empty})
       constEnv)

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

data ConstEnv = ConstEnv
  { envForeignFuns :: IntMap Int64
  , envStrings :: IntMap Int64
  , envLabels :: IntMap Int64
  }

data Env = Env
  { envCodeOffset :: Int64
  , envNewLables :: IntMap Int64
  }

type Translator = StateT Env (ReaderT ConstEnv (Writer BSBuilder))

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

rexR :: REX
rexR = mempty {rex_R = True}

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

regXMM :: RegisterXMM -> (Bool, Word8)
regXMM RegisterXMM0 = (False, 0)
regXMM RegisterXMM1 = (False, 1)
regXMM RegisterXMM2 = (False, 2)
regXMM RegisterXMM3 = (False, 3)
regXMM RegisterXMM4 = (False, 4)
regXMM RegisterXMM5 = (False, 5)
regXMM RegisterXMM6 = (False, 6)
regXMM RegisterXMM7 = (False, 7)

data ModRM_R
  = ModRM_R_Ext Word8
  | ModRM_R_Register Register
  | ModRM_R_RegisterXMM RegisterXMM

modRM_R :: ModRM_R -> (Bool, Word8)
modRM_R (ModRM_R_Ext v) = (False, v)
modRM_R (ModRM_R_Register r) = reg r
modRM_R (ModRM_R_RegisterXMM r) = regXMM r

data ModRM_RM
  = ModRM_RM_Register Register
  | ModRM_RM_RegisterXMM RegisterXMM
  | ModRM_RM_Pointer Pointer

intOperandToRM :: IntOperand -> ModRM_RM
intOperandToRM (IntOperandRegister _ rm) = ModRM_RM_Register rm
intOperandToRM (IntOperandPointer p) = ModRM_RM_Pointer p

modRM_RM :: ModRM_RM -> (Word8, Bool, Word8, Maybe Word8, Maybe Word32)
modRM_RM (ModRM_RM_Register rm) =
  let (rmExt, rmBits) = reg rm
   in (3, rmExt, rmBits, Nothing, Nothing)
modRM_RM (ModRM_RM_RegisterXMM rm) =
  let (rmExt, rmBits) = regXMM rm
   in (3, rmExt, rmBits, Nothing, Nothing)
modRM_RM (ModRM_RM_Pointer p) = go (pointerBase p) (pointerDisplacement p)
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

modRM :: ModRM_R -> ModRM_RM -> (REX, Word8, Maybe Word8, Maybe Word32)
modRM r rm =
  let (rExt, rBits) = modRM_R r
      (mode, rmExt, rmBits, sib, extra) = modRM_RM rm
   in ( mempty {rex_R = rExt, rex_B = rmExt}
      , mode * 64 + rBits * 8 + rmBits
      , sib
      , extra)

modRM8 :: Register8 -> Word8
modRM8 RegisterAL = 0xc0
modRM8 RegisterCL = 0xc1
modRM8 RegisterDL = 0xc2

incrementLocation :: (MonadState Env m) => Int64 -> m ()
incrementLocation i =
  State.modify $ \env -> env {envCodeOffset = envCodeOffset env + i}

byte :: (MonadWriter BSBuilder m, MonadState Env m) => Word8 -> m ()
byte x = Writer.tell (BSBuilder.word8 x) >> incrementLocation 1

word32 :: (MonadWriter BSBuilder m, MonadState Env m) => Word32 -> m ()
word32 x = Writer.tell (BSBuilder.word32BE x) >> incrementLocation 4

int32 :: (MonadWriter BSBuilder m, MonadState Env m) => Int32 -> m ()
int32 x = Writer.tell (BSBuilder.int32BE x) >> incrementLocation 4

int64 :: (MonadWriter BSBuilder m, MonadState Env m) => Int64 -> m ()
int64 x = Writer.tell (BSBuilder.int64BE x) >> incrementLocation 8

double :: (MonadWriter BSBuilder m, MonadState Env m) => Double -> m ()
double x = Writer.tell (BSBuilder.doubleBE x) >> incrementLocation 8

instructionWithModRM :: REX -> [Word8] -> ModRM_R -> ModRM_RM -> Translator ()
instructionWithModRM x i r rm = do
  let (x', modRMByte, sibByte, extra) = modRM r rm
      x'' = x `mappend` x'
  byte $ rexByte x''
  mapM_ byte i
  byte $ modRMByte
  maybe (pure ()) byte sibByte
  maybe (pure ()) word32 extra

instructionWithModRM8 :: [Word8] -> Register8 -> Translator ()
instructionWithModRM8 i r = do
  mapM_ byte i
  byte $ modRM8 r

resolveRelativeLocation :: Int64 -> Translator ()
resolveRelativeLocation loc = do
  currentLocation <- State.gets envCodeOffset
  -- Our current location is before we place the address, but relative
  -- addressing uses location just after the address.
  let relativeLocation = loc - (currentLocation + 4)
  int32 $ fromIntegral relativeLocation

resolveLabel :: LabelID -> Translator ()
resolveLabel (LabelID lid) = do
  ls <- Reader.asks envLabels
  case (IntMap.lookup lid ls) of
    Nothing -> int32 0
    Just loc -> resolveRelativeLocation loc

resolveFunction :: FunctionCall -> Translator ()
resolveFunction NativeFunctionCall {nativeFunCallName = l} = resolveLabel l
resolveFunction ForeignFunctionCall {foreignFunCallName = FunID fid} = do
  fs <- Reader.asks envForeignFuns
  resolveRelativeLocation (fs IntMap.! fid)

resolveString :: StringID -> Translator ()
resolveString (StringID sid) = do
  ss <- Reader.asks envStrings
  resolveRelativeLocation (ss IntMap.! sid)

introduceLabel :: LabelID -> Translator ()
introduceLabel (LabelID lid) = do
  State.modify $ \env ->
    env
      {envNewLables = IntMap.insert lid (envCodeOffset env) (envNewLables env)}

translateInstruction :: Instruction -> Translator ()
translateInstruction (InstructionCMP r rm) =
  instructionWithModRM rexW [0x3b] (ModRM_R_Register r) (intOperandToRM rm)
translateInstruction (InstructionSetZ r) = instructionWithModRM8 [0x0f, 0x94] r
translateInstruction (InstructionSetNZ r) = instructionWithModRM8 [0x0f, 0x95] r
translateInstruction (InstructionSetS r) = instructionWithModRM8 [0x0f, 0x98] r
translateInstruction (InstructionSetC r) = instructionWithModRM8 [0x0f, 0x92] r
translateInstruction (InstructionMOV_R64_IMM64 r imm) = do
  let (rExt, rBits) = reg r
  byte $ rexByte $ rexW {rex_R = rExt}
  byte $ 0xb8 + rBits
  case imm of
    ImmediateInt i -> int64 i
    ImmediateFloat d -> double d
translateInstruction (InstructionMOV_R64_RM64 r rm) =
  instructionWithModRM rexW [0x8b] (ModRM_R_Register r) (intOperandToRM rm)
translateInstruction (InstructionMOV_RM64_R64 rm r) =
  instructionWithModRM rexW [0x89] (ModRM_R_Register r) (intOperandToRM rm)
translateInstruction (InstructionLabelledNOP lid) = do
  introduceLabel lid
  byte 0x90
translateInstruction (InstructionJMP lid) = do
  byte 0xe9
  resolveLabel lid
translateInstruction (InstructionJZ lid) = do
  byte 0x0f
  byte 0x84
  resolveLabel lid
translateInstruction InstructionRET = byte 0xc3
translateInstruction (InstructionCALL fcall) = do
  byte 0xe8
  resolveFunction fcall
translateInstruction (InstructionNEG rm) =
  instructionWithModRM rexW [0xf7] (ModRM_R_Ext 3) (intOperandToRM rm)
translateInstruction (InstructionAND r rm) =
  instructionWithModRM rexW [0x23] (ModRM_R_Register r) (intOperandToRM rm)
translateInstruction (InstructionXOR r rm) =
  instructionWithModRM rexW [0x33] (ModRM_R_Register r) (intOperandToRM rm)
translateInstruction (InstructionOR r rm) =
  instructionWithModRM rexW [0x0b] (ModRM_R_Register r) (intOperandToRM rm)
translateInstruction (InstructionADD r rm) =
  instructionWithModRM rexW [0x03] (ModRM_R_Register r) (intOperandToRM rm)
translateInstruction (InstructionSUB r rm) =
  instructionWithModRM rexW [0x2b] (ModRM_R_Register r) (intOperandToRM rm)
translateInstruction (InstructionIDIV rm) =
  instructionWithModRM rexW [0xf7] (ModRM_R_Ext 7) (intOperandToRM rm)
translateInstruction (InstructionIMUL r rm) =
  instructionWithModRM
    rexW
    [0x0f, 0xaf]
    (ModRM_R_Register r)
    (intOperandToRM rm)
translateInstruction InstructionCQO = do
  byte $ rexByte rexW
  byte 0x99
translateInstruction (InstructionADDSD r rm) =
  instructionWithModRM
    mempty
    [0xf2, 0x0f, 0x58]
    (ModRM_R_RegisterXMM r)
    (ModRM_RM_RegisterXMM rm)
translateInstruction (InstructionSUBSD r rm) =
  instructionWithModRM
    mempty
    [0xf2, 0x0f, 0x5c]
    (ModRM_R_RegisterXMM r)
    (ModRM_RM_RegisterXMM rm)
translateInstruction (InstructionMULSD r rm) =
  instructionWithModRM
    mempty
    [0xf2, 0x0f, 0x59]
    (ModRM_R_RegisterXMM r)
    (ModRM_RM_RegisterXMM rm)
translateInstruction (InstructionDIVSD r rm) =
  instructionWithModRM
    mempty
    [0xf2, 0x0f, 0x5e]
    (ModRM_R_RegisterXMM r)
    (ModRM_RM_RegisterXMM rm)
translateInstruction (InstructionCOMISD r rm) =
  instructionWithModRM
    mempty
    [0x66, 0x0f, 0x2f]
    (ModRM_R_RegisterXMM r)
    (ModRM_RM_RegisterXMM rm)
translateInstruction (InstructionMOVSD_XMM_XMM r rm) =
  instructionWithModRM
    mempty
    [0xf2, 0x0f, 0x10]
    (ModRM_R_RegisterXMM r)
    (ModRM_RM_RegisterXMM rm)
translateInstruction (InstructionMOVSD_XMM_M64 r rm) =
  instructionWithModRM
    mempty
    [0xf2, 0x0f, 0x10]
    (ModRM_R_RegisterXMM r)
    (ModRM_RM_Pointer rm)
translateInstruction (InstructionMOVSD_M64_XMM rm r) =
  instructionWithModRM
    mempty
    [0xf2, 0x0f, 0x11]
    (ModRM_R_RegisterXMM r)
    (ModRM_RM_Pointer rm)
translateInstruction (InstructionCVTSI2SD r rm) = do
  byte 0xf2
  instructionWithModRM
    rexW
    [0x0f, 0x2a]
    (ModRM_R_RegisterXMM r)
    (intOperandToRM rm)
translateInstruction (InstructionPUSH r) = do
  let (rExt, rBits) = reg r
  when rExt $ byte $ rexByte rexR
  byte $ 0x50 + rBits
translateInstruction (InstructionPOP rm) =
  instructionWithModRM mempty [0x8f] (ModRM_R_Ext 0) (intOperandToRM rm)
translateInstruction (InstructionLEA r s) = do
  let (rExt, rBits) = reg r
      (x, modRMByte) = (rexW {rex_R = rExt}, rBits * 8 + 5)
  byte $ rexByte x
  byte 0x8d
  byte modRMByte
  resolveString s
