module PrettyPrintBinary
  ( prettyPrint
  ) where

import Data.Int (Int64)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Word (Word8)
import qualified Foreign.C.String as Foreign
import Foreign.Ptr (IntPtr(..), Ptr, castPtr, intPtrToPtr, plusPtr)
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
prettyPrintCode ls p l = go (convertLabelMap ls) (castPtr p) l 0
  where
    go :: [Ptr Word8] -> Ptr Word8 -> Int64 -> Int -> IO String
    go _ _ 0 _ = pure ""
    go labels ptr len 10 = ("\n" ++) <$> go labels ptr len 0
    go labels ptr len i =
      let f =
            if ptr `elem` labels
              then (\x xs -> "\nlabel:\n" ++ x ++ xs)
              else (++)
       in f <$> printByte ptr <*> go labels (ptr `plusPtr` 1) (len - 1) (i + 1)
    printByte :: Ptr Word8 -> IO String
    printByte ptr = do
      b <- peek ptr
      pure $ printf "%x " b
    convertLabelMap = map (int64ToPtr) . IntMap.elems
