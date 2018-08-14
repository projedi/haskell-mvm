module PrettyPrintBinary
  ( prettyPrint
  ) where

import Data.Int (Int64)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Word (Word8)
import qualified Foreign.C.String as Foreign
import Foreign.Ptr (IntPtr(..), Ptr, castPtr, intPtrToPtr)
import Foreign.Storable (peek)
import Text.Printf (printf)

import BinarySyntax

prettyPrint :: Program -> IO String
prettyPrint p = do
  fs <- prettyPrintFunctions $ programForeignFunctions p
  ss <- prettyPrintStrings $ programStrings p
  code <- prettyPrintCode (programLabels p) (programCode p) (programSize p)
  pure $ unlines ["Functions:", fs, "Strings:", ss, "Code:", code]

int64ToPtr :: Int64 -> Ptr a
int64ToPtr p = intPtrToPtr (IntPtr (fromIntegral p))

prettyPrintStrings :: IntMap Int64 -> IO String
prettyPrintStrings ss = (unlines . IntMap.elems) <$> mapM go ss
  where
    go :: Int64 -> IO String
    go ptr = do
      s <- Foreign.peekCString (int64ToPtr ptr)
      pure $ show ptr ++ ": " ++ show s

prettyPrintFunctions :: IntMap Int64 -> IO String
prettyPrintFunctions fs = (unlines . IntMap.elems) <$> mapM go fs
  where
    go = pure . show

prettyPrintCode :: IntMap Int64 -> Ptr () -> Int64 -> IO String
prettyPrintCode ls p l = go (convertLabelMap ls) (castPtr p) l
  where
    go :: IntMap (Ptr Word8) -> Ptr Word8 -> Int64 -> IO String
    go labels ptr len = _
    convertLabelMap =
      IntMap.fromList .
      map (\(v1, v2) -> (fromIntegral v2, int64ToPtr $ fromIntegral v1)) .
      IntMap.toList
