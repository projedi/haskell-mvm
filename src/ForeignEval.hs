{-# LANGUAGE Rank2Types, ScopedTypeVariables #-}

module ForeignEval
  ( LibHandle()
  , ForeignFun()
  , dlclose
  , dlopen
  , call
  , findSymbol
  ) where

import Control.Exception (catch, tryJust)
import qualified Foreign.LibFFI as FFI
import Foreign.Ptr (FunPtr())
import System.IO.Error (IOError, ioeGetErrorString)
import qualified System.Posix.DynamicLinker as DL

import Syntax (VarType(..))
import Value

newtype LibHandle =
  LibHandle DL.DL

newtype ForeignFun =
  ForeignFun (FunPtr ())

dlopen :: String -> IO (Either String LibHandle)
dlopen name =
  tryJust (Just . ioeGetErrorString) $
  LibHandle <$> DL.dlopen name [DL.RTLD_LAZY]

dlclose :: LibHandle -> IO ()
dlclose (LibHandle handle) = DL.dlclose handle

findSymbol :: String -> IO (Maybe ForeignFun)
findSymbol name =
  catch ((Just . ForeignFun) <$> DL.dlsym DL.Default name) $
  \(_ :: IOError) -> pure Nothing

withValue :: Value -> (FFI.Arg -> IO a) -> IO a
withValue (ValueInt i) fun = fun (FFI.argInt i)
withValue (ValueFloat f) fun = fun (FFI.argCDouble $ realToFrac f)
withValue (ValueString s) fun = fun (FFI.argString s)

withValues :: [Value] -> ([FFI.Arg] -> IO a) -> IO a
withValues [] f = f []
withValues (v:vs) f = withValue v $ \a -> withValues vs $ \as -> f (a : as)

call' :: Maybe VarType -> (forall a. FFI.RetType a -> IO a) -> IO (Maybe Value)
call' Nothing fun = fun FFI.retVoid >> pure Nothing
call' (Just VarTypeInt) fun = (Just . ValueInt) <$> fun FFI.retInt
call' (Just VarTypeFloat) fun = (Just . ValueFloat . realToFrac) <$> fun FFI.retCDouble
-- The returned char* is not freed. So it might be leaky.
call' (Just VarTypeString) fun = (Just . ValueString) <$> fun FFI.retString

call :: ForeignFun -> Maybe VarType -> [Value] -> IO (Maybe Value)
call (ForeignFun fun) rettype vals =
  withValues vals $ \args -> call' rettype (\r -> FFI.callFFI fun r args)
