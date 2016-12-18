{-# LANGUAGE MultiParamTypeClasses, QuasiQuotes, TemplateHaskell
  #-}

module ForeignEval
  ( LibHandle()
  , ForeignFun()
  , dlclose
  , dlopen
  , call
  , findSymbol
  ) where

import Control.Monad
import qualified Foreign.C.String as CString
import Foreign.C.Types (CDouble)
import Foreign.Ptr (Ptr, nullPtr)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU
import System.IO (hPutStrLn, stderr)

import Syntax (VarType(..))
import Value

C.include "<dlfcn.h>"

newtype LibHandle =
  LibHandle (Ptr ())

newtype ForeignFun =
  ForeignFun (Ptr ())

{-# ANN c_dlopen "HLint: ignore Use camelCase" #-}

c_dlopen :: String -> IO LibHandle
c_dlopen name =
  CString.withCString name $
  \cname ->
     LibHandle <$> [CU.exp| void* { dlopen($(const char *cname), RTLD_LAZY) } |]

{-# ANN c_dlclose "HLint: ignore Use camelCase" #-}

c_dlclose :: LibHandle -> IO Bool
c_dlclose (LibHandle handle) =
  (== 0) <$> [CU.exp| int { dlclose($(void *handle)) } |]

{-# ANN c_dlerror "HLint: ignore Use camelCase" #-}

c_dlerror :: IO String
c_dlerror = [CU.exp| char* { dlerror() } |] >>= CString.peekCString

{-# ANN c_dlsym "HLint: ignore Use camelCase" #-}

c_dlsym :: LibHandle -> String -> IO ForeignFun
c_dlsym (LibHandle handle) name =
  CString.withCString name $
  \cname ->
     ForeignFun <$> [CU.exp| void* { dlsym($(void *handle), $(char* cname)) } |]

dlopen :: String -> IO (Either String LibHandle)
dlopen lib = do
  (LibHandle handle) <- c_dlopen lib
  if handle /= nullPtr
    then pure $ Right $ LibHandle handle
    else Left <$> c_dlerror

dlclose :: LibHandle -> IO ()
dlclose handle = do
  res <- c_dlclose handle
  unless res $
    do err <- c_dlerror
       hPutStrLn stderr $ "WARNING: " ++ err

findSymbol :: [LibHandle] -> String -> IO (Maybe ForeignFun)
findSymbol [] _ = pure Nothing
findSymbol (lib:libs) name = do
  (ForeignFun res) <- c_dlsym lib name
  if res /= nullPtr
    then pure $ Just $ ForeignFun res
    else findSymbol libs name

class CallForeignFun args ret  where
  callForeignFun :: ForeignFun -> args -> IO ret

instance CallForeignFun CDouble CDouble where
  callForeignFun (ForeignFun f) a0 =
    [CU.exp| double { (*((double(*)(double))($(void* f))))($(double a0)) } |]

call :: ForeignFun -> Maybe VarType -> [Value] -> IO (Maybe Value)
call fun rettype vals =
  case (rettype, vals) of
    (Just VarTypeFloat, [ValueFloat f]) ->
      callForeignFun fun (realToFrac f :: CDouble) >>= floatReturnValue
    _ -> error "Extend supported FFI calls as needed"
  where
    floatReturnValue :: CDouble -> IO (Maybe Value)
    floatReturnValue = pure . Just . ValueFloat . realToFrac
