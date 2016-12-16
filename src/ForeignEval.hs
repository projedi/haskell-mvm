{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module ForeignEval where

import Control.Monad
import qualified Foreign.C.String as CString
import Foreign.Ptr (Ptr, nullPtr)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU
import System.IO (hPutStrLn, stderr)

C.include "<dlfcn.h>"

newtype LibHandle = LibHandle (Ptr ())

c_dlopen :: String -> IO LibHandle
c_dlopen name = CString.withCString name $ \cname ->
  LibHandle <$> [CU.exp| void* { dlopen($(const char *cname), RTLD_LAZY) } |]

c_dlclose :: LibHandle -> IO Bool
c_dlclose (LibHandle handle) =
  (== 0) <$> [CU.exp| int { dlclose($(void *handle)) } |]

c_dlerror :: IO String
c_dlerror = [CU.exp| char* { dlerror() } |] >>= CString.peekCString

c_dlsym :: LibHandle -> String -> IO (Ptr ())
c_dlsym (LibHandle handle) name = CString.withCString name $ \cname ->
  [CU.exp| void* { dlsym($(void *handle), $(char* cname)) } |]

dlopen :: String -> IO (Either String LibHandle)
dlopen lib = do
  (LibHandle handle) <- c_dlopen lib
  if (handle /= nullPtr)
    then pure $ Right $ LibHandle handle
    else Left <$> c_dlerror

dlclose :: LibHandle -> IO ()
dlclose handle = do
  res <- c_dlclose handle
  unless res $ do
    err <- c_dlerror
    hPutStrLn stderr $ "WARNING: " ++ err
