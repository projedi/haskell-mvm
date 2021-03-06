module CallingConvention
  ( FunctionCall(..)
  , CallingConvention(..)
  , ArgLocation(..)
  , RetLocation(..)
  , computeCallingConvention
  ) where

import Control.Monad.State (State, runState)
import qualified Control.Monad.State as State
import Data.Int (Int64)

import ASMSyntax (Register(..), RegisterXMM(..), VarType(..), typeSize)

data FunctionCall = FunctionCall
  { funRetType :: Maybe VarType
  , funArgTypes :: [VarType]
  }

data ArgLocation
  = ArgLocationRegister VarType
                        Register
  | ArgLocationRegisterXMM RegisterXMM
  | ArgLocationStack VarType
                     Int64

data RetLocation
  = RetLocationRegister VarType
                        Register
  | RetLocationRegisterXMM RegisterXMM

data CallingConvention = CallingConvention
  { funRetValue :: Maybe RetLocation
  , funArgValues :: [ArgLocation]
  , funStackToAllocate :: [VarType] -- The top should be the last.
  , funFloatRegistersUsed :: Int
  }

computeCallingConvention :: FunctionCall -> CallingConvention
computeCallingConvention fcall =
  CallingConvention
    { funRetValue = putReturnValue <$> funRetType fcall
    , funArgValues = argValues
    , funStackToAllocate = stackToAllocate
    , funFloatRegistersUsed = floatRegisterUsage
    }
  where
    (argValues, stackToAllocate, floatRegisterUsage) =
      putArgs $ funArgTypes fcall

putReturnValue :: VarType -> RetLocation
putReturnValue t@VarTypeInt = RetLocationRegister t RegisterRAX
putReturnValue t@(VarTypePtr _) = RetLocationRegister t RegisterRAX
putReturnValue t@VarTypeString = RetLocationRegister t RegisterRAX
putReturnValue VarTypeFloat = RetLocationRegisterXMM RegisterXMM0

putArgs :: [VarType] -> ([ArgLocation], [VarType], Int)
putArgs args = (ops, stack finalEnv, floatUsage finalEnv)
  where
    (ops, finalEnv) =
      runState
        (mapM putArg args)
        Env
          { availableIntRegisters =
              [ RegisterRDI
              , RegisterRSI
              , RegisterRDX
              , RegisterRCX
              , RegisterR8
              , RegisterR9
              ]
          , availableFloatRegisters =
              [ RegisterXMM0
              , RegisterXMM1
              , RegisterXMM2
              , RegisterXMM3
              , RegisterXMM4
              , RegisterXMM5
              , RegisterXMM6
              , RegisterXMM7
              ]
          , stack = []
          , nextOffset = 0
          , floatUsage = 0
          }

type ComputeArg = State Env

data Env = Env
  { availableIntRegisters :: [Register]
  , availableFloatRegisters :: [RegisterXMM]
  , stack :: [VarType]
  , nextOffset :: Int64
  , floatUsage :: Int
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
      State.modify
        (\env ->
           env {availableFloatRegisters = rs, floatUsage = 1 + floatUsage env})
      pure $ ArgLocationRegisterXMM r

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
  State.modify $ \env ->
    env {stack = t : stack env, nextOffset = offset + typeSize t}
  pure $ ArgLocationStack t offset
