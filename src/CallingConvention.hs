module CallingConvention
  ( FunctionCall(..)
  , CallingConvention(..)
  , ArgLocation(..)
  , computeCallingConvention
  ) where

import Control.Monad.State (State, runState)
import qualified Control.Monad.State as State
import Data.Int (Int64)

import ASMSyntax (Register(..), VarType(..))

data FunctionCall = FunctionCall
  { funRetType :: Maybe VarType
  , funArgTypes :: [VarType]
  }

data ArgLocation
  = ArgLocationRegister VarType
                        Register
  | ArgLocationStack VarType
                     Int64

data CallingConvention = CallingConvention
  { funRetValue :: Maybe (VarType, Register)
  , funArgValues :: [ArgLocation]
  , funStackToAllocate :: [VarType] -- The top should be the last.
  }

computeCallingConvention :: FunctionCall -> CallingConvention
computeCallingConvention fcall =
  CallingConvention
    { funRetValue = (\t -> (t, putReturnValue t)) <$> funRetType fcall
    , funArgValues = argValues
    , funStackToAllocate = stackToAllocate
    }
  where
    (argValues, stackToAllocate) = putArgs $ funArgTypes fcall

putReturnValue :: VarType -> Register
putReturnValue VarTypeInt = RegisterRAX
putReturnValue (VarTypePtr _) = RegisterRAX
putReturnValue VarTypeString = RegisterRAX
putReturnValue VarTypeFloat = RegisterXMM0

putArgs :: [VarType] -> ([ArgLocation], [VarType])
putArgs args = (ops, stack finalEnv)
  where
    (ops, finalEnv) =
      runState
        (mapM putArg args)
        Env
          { availableIntRegisters = []
          , availableFloatRegisters = []
          , stack = []
          , nextOffset = 0
          }

type ComputeArg = State Env

data Env = Env
  { availableIntRegisters :: [Register]
  , availableFloatRegisters :: [Register]
  , stack :: [VarType]
  , nextOffset :: Int64
  }

putArg :: VarType -> ComputeArg ArgLocation
putArg VarTypeFloat = putFloatArg
putArg t = putIntArg t

putFloatArg :: ComputeArg ArgLocation
putFloatArg = do
  regs <- State.gets availableFloatRegisters
  case regs of
    [] -> putArgOnStack VarTypeFloat
    (r:rs) -> do
      State.modify (\env -> env {availableFloatRegisters = rs})
      pure $ ArgLocationRegister VarTypeFloat r

putIntArg :: VarType -> ComputeArg ArgLocation
putIntArg t = do
  regs <- State.gets availableIntRegisters
  case regs of
    [] -> putArgOnStack t
    (r:rs) -> do
      State.modify (\env -> env {availableIntRegisters = rs})
      pure $ ArgLocationRegister t r

putArgOnStack :: VarType -> ComputeArg ArgLocation
putArgOnStack t = do
  offset <- State.gets nextOffset
  State.modify $ \env -> env {stack = t : stack env, nextOffset = offset + 1}
  pure $ ArgLocationStack t offset
