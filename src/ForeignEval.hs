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
import qualified System.Info
import qualified System.Posix.DynamicLinker as DL

import TypedSyntax (VarType(..))
import Value

newtype LibHandle =
  LibHandle DL.DL

newtype ForeignFun =
  ForeignFun (FunPtr ())

getLibName :: String -> String
getLibName "libm" =
  case System.Info.os of
    "darwin" -> "libm.dylib"
    _ -> "libm.so.6"
getLibName lib =
  error $ "Cannot find " ++ lib ++ ": proper lib search is not yet implemented"

dlopen :: String -> IO (Either String LibHandle)
dlopen name =
  tryJust (Just . ioeGetErrorString) $
  LibHandle <$> DL.dlopen (getLibName name) [DL.RTLD_LAZY]

dlclose :: LibHandle -> IO ()
dlclose (LibHandle handle) = DL.dlclose handle

findSymbol :: String -> IO (Maybe ForeignFun)
findSymbol name =
  catch (Just . ForeignFun <$> DL.dlsym DL.Default name) $ \(_ :: IOError) ->
    pure Nothing

withValue :: Value -> (FFI.Arg -> IO a) -> IO a
withValue (ValueInt i) fun = fun (FFI.argInt64 i)
withValue (ValueFloat f) fun = fun (FFI.argCDouble $ realToFrac f)
-- If it's from literal, make a copy.
withValue (ValueString (Right s)) fun = fun (FFI.argString s)
-- It it's not, pass it directly.
withValue (ValueString (Left cs)) fun = fun (FFI.argPtr cs)
withValue (ValuePtr _ _) _ = error "Type mismatch"

withValues :: [Value] -> ([FFI.Arg] -> IO a) -> IO a
withValues [] f = f []
withValues (v:vs) f = withValue v $ \a -> withValues vs $ \as -> f (a : as)

call' :: Maybe VarType -> (forall a. FFI.RetType a -> IO a) -> IO (Maybe Value)
call' Nothing fun = fun FFI.retVoid >> pure Nothing
call' (Just VarTypeInt) fun = Just . ValueInt <$> fun FFI.retInt64
call' (Just VarTypeFloat) fun =
  Just . ValueFloat . realToFrac <$> fun FFI.retCDouble
call' (Just VarTypeString) fun =
  Just . ValueString . Left <$> fun FFI.retCString
call' (Just (VarTypePtr _)) _ = error "Type mismatch"

call :: ForeignFun -> Maybe VarType -> [Value] -> IO (Maybe Value)
call (ForeignFun fun) rettype vals =
  withValues vals $ \args -> call' rettype (\r -> FFI.callFFI fun r args)
