{-# LANGUAGE ViewPatterns #-}

module TypeChecker
  ( typeCheck
  ) where

import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Reader as Reader
import Control.Monad.State (State, runState)
import qualified Control.Monad.State as State
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import qualified ResolvedSyntax
import qualified TypedSyntax
import Value (Value)
import qualified Value

typeCheck :: ResolvedSyntax.Program -> TypedSyntax.Program
typeCheck p =
  TypedSyntax.Program
    { TypedSyntax.programFunctions = fs
    , TypedSyntax.programLibraries = ResolvedSyntax.programLibraries p
    , TypedSyntax.programForeignFunctions = envForeignFunctions finalEnv
    , TypedSyntax.programConstants = envConsts finalEnv
    , TypedSyntax.programVariables = ResolvedSyntax.programVariables p
    , TypedSyntax.programLastFunID = ResolvedSyntax.programLastFunID p
    , TypedSyntax.programLastVarID = ResolvedSyntax.programLastVarID p
    , TypedSyntax.programLastConstID = ResolvedSyntax.programLastConstID p
    }
  where
    startEnv =
      Env
        { envFuns = ResolvedSyntax.programFunctions p
        , envForeignFunctions = ResolvedSyntax.programForeignFunctions p
        , envConsts = ResolvedSyntax.programConstants p
        }
    (fs, finalEnv) = runTypeChecker p startEnv

data ConstEnv = ConstEnv
  { constEnvRetType :: Maybe TypedSyntax.VarType
  , constEnvVars :: IntMap TypedSyntax.VarType
  }

data Env = Env
  { envFuns :: IntMap ResolvedSyntax.FunctionDef
  , envForeignFunctions :: IntMap ResolvedSyntax.ForeignFunctionDecl
  , envConsts :: IntMap Value
  }

type TypeChecker = ReaderT ConstEnv (State Env)

runTypeChecker ::
     ResolvedSyntax.Program -> Env -> (IntMap TypedSyntax.FunctionDef, Env)
runTypeChecker p = runState (traverse go $ ResolvedSyntax.programFunctions p)
  where
    constEnv f =
      ConstEnv
        { constEnvRetType = ResolvedSyntax.funDefRetType f
        , constEnvVars = ResolvedSyntax.programVariables p
        }
    go f =
      flip runReaderT (constEnv f) $ do
        body <- typecheckBlock $ ResolvedSyntax.funDefBody f
        pure
          TypedSyntax.FunctionDef
            { TypedSyntax.funDefRetType = ResolvedSyntax.funDefRetType f
            , TypedSyntax.funDefName = ResolvedSyntax.funDefName f
            , TypedSyntax.funDefParams = ResolvedSyntax.funDefParams f
            , TypedSyntax.funDefCaptures = ResolvedSyntax.funDefCaptures f
            , TypedSyntax.funDefBody = body
            }

getVariableType ::
     ResolvedSyntax.VarID -> TypeChecker (Maybe TypedSyntax.VarType)
getVariableType (TypedSyntax.VarID v) =
  Reader.asks (IntMap.lookup v . constEnvVars)

typecheckBlock :: ResolvedSyntax.Block -> TypeChecker TypedSyntax.Block
typecheckBlock block = do
  stmts <- mapM typecheckStatement (ResolvedSyntax.blockStatements block)
  pure TypedSyntax.Block {TypedSyntax.blockStatements = stmts}

noopBlock :: TypedSyntax.Block
noopBlock = TypedSyntax.Block {TypedSyntax.blockStatements = []}

typecheckStatement ::
     ResolvedSyntax.Statement -> TypeChecker TypedSyntax.Statement
typecheckStatement (ResolvedSyntax.StatementBlock block) =
  TypedSyntax.StatementBlock <$> typecheckBlock block
typecheckStatement (ResolvedSyntax.StatementVarAlloc v) =
  pure $ TypedSyntax.StatementVarAlloc v
typecheckStatement (ResolvedSyntax.StatementFunctionCall fcall) =
  TypedSyntax.StatementFunctionCall <$> typecheckFunctionCall fcall
typecheckStatement (ResolvedSyntax.StatementWhile e block) =
  TypedSyntax.StatementWhile <$> typecheckExpr e <*> typecheckBlock block
typecheckStatement (ResolvedSyntax.StatementAssign v e) = do
  Just t <- getVariableType v
  TypedSyntax.StatementAssign v . convertExprTo t <$> typecheckExpr e
typecheckStatement (ResolvedSyntax.StatementAssignPlus v e) =
  typecheckStatement (ResolvedSyntax.StatementAssign v e')
  where
    e' = ResolvedSyntax.ExprPlus (ResolvedSyntax.ExprVar v) e
typecheckStatement (ResolvedSyntax.StatementAssignMinus v e) =
  typecheckStatement (ResolvedSyntax.StatementAssign v e')
  where
    e' = ResolvedSyntax.ExprMinus (ResolvedSyntax.ExprVar v) e
typecheckStatement (ResolvedSyntax.StatementIfElse e bt bf) =
  TypedSyntax.StatementIfElse <$> typecheckExpr e <*> typecheckBlock bt <*>
  typecheckBlock bf
typecheckStatement (ResolvedSyntax.StatementIf e bt) =
  TypedSyntax.StatementIfElse <$> typecheckExpr e <*> typecheckBlock bt <*>
  pure noopBlock
typecheckStatement (ResolvedSyntax.StatementReturn Nothing) = do
  Nothing <- Reader.asks constEnvRetType
  pure $ TypedSyntax.StatementReturn Nothing
typecheckStatement (ResolvedSyntax.StatementReturn (Just e)) = do
  Just expectedType <- Reader.asks constEnvRetType
  TypedSyntax.StatementReturn . Just . convertExprTo expectedType <$>
    typecheckExpr e

convertExprTo :: TypedSyntax.VarType -> TypedSyntax.Expr -> TypedSyntax.Expr
convertExprTo TypedSyntax.VarTypeInt e@(TypedSyntax.exprType -> TypedSyntax.VarTypeInt) =
  e
convertExprTo TypedSyntax.VarTypeFloat e@(TypedSyntax.exprType -> TypedSyntax.VarTypeFloat) =
  e
convertExprTo TypedSyntax.VarTypeString e@(TypedSyntax.exprType -> TypedSyntax.VarTypeString) =
  e
convertExprTo TypedSyntax.VarTypeFloat e@(TypedSyntax.exprType -> TypedSyntax.VarTypeInt) =
  TypedSyntax.ExprUnOp TypedSyntax.UnIntToFloat e
convertExprTo _ _ = error "Type mismatch"

convertArgsTo ::
     [TypedSyntax.VarType] -> [TypedSyntax.Expr] -> [TypedSyntax.Expr]
convertArgsTo [] [] = []
convertArgsTo (t:ts) (e:es) = convertExprTo t e : convertArgsTo ts es
convertArgsTo _ _ = error "Type mismatch"

typecheckFunctionCall ::
     ResolvedSyntax.FunctionCall -> TypeChecker TypedSyntax.FunctionCall
typecheckFunctionCall (ResolvedSyntax.NativeFunctionCall (TypedSyntax.FunID fid) args) = do
  Just fdef <- State.gets (IntMap.lookup fid . envFuns)
  args' <-
    convertArgsTo
      (map (\(ResolvedSyntax.VarDecl t _) -> t) $
       ResolvedSyntax.funDefParams fdef) <$>
    mapM typecheckExpr args
  pure $
    TypedSyntax.NativeFunctionCall
      { TypedSyntax.nativeFunCallName = ResolvedSyntax.funDefName fdef
      , TypedSyntax.nativeFunCallRetType = ResolvedSyntax.funDefRetType fdef
      , TypedSyntax.nativeFunCallArgs = args'
      }
typecheckFunctionCall (ResolvedSyntax.ForeignFunctionCall (TypedSyntax.FunID fid) args) = do
  Just fdef <- State.gets (IntMap.lookup fid . envForeignFunctions)
  args' <-
    convertArgsTo (ResolvedSyntax.foreignFunDeclParams fdef) <$>
    mapM typecheckExpr args
  pure $
    TypedSyntax.ForeignFunctionCall
      { TypedSyntax.foreignFunCallName = ResolvedSyntax.foreignFunDeclName fdef
      , TypedSyntax.foreignFunCallRetType =
          ResolvedSyntax.foreignFunDeclRetType fdef
      , TypedSyntax.foreignFunCallArgs = args'
      }
typecheckFunctionCall (ResolvedSyntax.PrintCall args) =
  TypedSyntax.PrintCall <$> mapM typecheckExpr args

selectBinOpType ::
     TypedSyntax.VarType -> TypedSyntax.VarType -> TypedSyntax.VarType
selectBinOpType TypedSyntax.VarTypeInt TypedSyntax.VarTypeInt =
  TypedSyntax.VarTypeInt
selectBinOpType TypedSyntax.VarTypeFloat TypedSyntax.VarTypeFloat =
  TypedSyntax.VarTypeFloat
selectBinOpType TypedSyntax.VarTypeString TypedSyntax.VarTypeString =
  TypedSyntax.VarTypeString
selectBinOpType TypedSyntax.VarTypeInt TypedSyntax.VarTypeFloat =
  TypedSyntax.VarTypeFloat
selectBinOpType TypedSyntax.VarTypeFloat TypedSyntax.VarTypeInt =
  TypedSyntax.VarTypeFloat
selectBinOpType _ _ = error "Type mismatch"

typecheckBinOp ::
     TypedSyntax.BinOp
  -> TypedSyntax.Expr
  -> TypedSyntax.Expr
  -> TypedSyntax.Expr
typecheckBinOp op lhs rhs =
  TypedSyntax.ExprBinOp op (convertExprTo t lhs) (convertExprTo t rhs)
  where
    t = selectBinOpType (TypedSyntax.exprType lhs) (TypedSyntax.exprType rhs)

typecheckExpr :: ResolvedSyntax.Expr -> TypeChecker TypedSyntax.Expr
typecheckExpr (ResolvedSyntax.ExprFunctionCall fcall) =
  TypedSyntax.ExprFunctionCall <$> typecheckFunctionCall fcall
typecheckExpr (ResolvedSyntax.ExprVar v) = do
  Just t <- getVariableType v
  pure $ TypedSyntax.ExprVar t v
typecheckExpr (ResolvedSyntax.ExprConst (TypedSyntax.ConstID cid)) = do
  Just v <- State.gets (IntMap.lookup cid . envConsts)
  pure $ TypedSyntax.ExprConst (Value.typeof v) (TypedSyntax.ConstID cid)
typecheckExpr (ResolvedSyntax.ExprNeg e) =
  TypedSyntax.ExprUnOp TypedSyntax.UnNeg <$> typecheckExpr e
typecheckExpr (ResolvedSyntax.ExprPlus lhs rhs) =
  typecheckBinOp TypedSyntax.BinPlus <$> typecheckExpr lhs <*> typecheckExpr rhs
typecheckExpr (ResolvedSyntax.ExprMinus lhs rhs) =
  typecheckBinOp TypedSyntax.BinMinus <$> typecheckExpr lhs <*>
  typecheckExpr rhs
typecheckExpr (ResolvedSyntax.ExprTimes lhs rhs) =
  typecheckBinOp TypedSyntax.BinTimes <$> typecheckExpr lhs <*>
  typecheckExpr rhs
typecheckExpr (ResolvedSyntax.ExprDiv lhs rhs) =
  typecheckBinOp TypedSyntax.BinDiv <$> typecheckExpr lhs <*> typecheckExpr rhs
typecheckExpr (ResolvedSyntax.ExprMod lhs rhs) =
  typecheckBinOp TypedSyntax.BinMod <$> typecheckExpr lhs <*> typecheckExpr rhs
typecheckExpr (ResolvedSyntax.ExprBitAnd lhs rhs) =
  typecheckBinOp TypedSyntax.BinBitAnd <$> typecheckExpr lhs <*>
  typecheckExpr rhs
typecheckExpr (ResolvedSyntax.ExprBitOr lhs rhs) =
  typecheckBinOp TypedSyntax.BinBitOr <$> typecheckExpr lhs <*>
  typecheckExpr rhs
typecheckExpr (ResolvedSyntax.ExprBitXor lhs rhs) =
  typecheckBinOp TypedSyntax.BinBitXor <$> typecheckExpr lhs <*>
  typecheckExpr rhs
typecheckExpr (ResolvedSyntax.ExprNot e) =
  TypedSyntax.ExprUnOp TypedSyntax.UnNot <$> typecheckExpr e
typecheckExpr (ResolvedSyntax.ExprAnd lhs rhs) =
  typecheckBinOp TypedSyntax.BinAnd <$> typecheckExpr lhs <*> typecheckExpr rhs
typecheckExpr (ResolvedSyntax.ExprOr lhs rhs) =
  typecheckBinOp TypedSyntax.BinOr <$> typecheckExpr lhs <*> typecheckExpr rhs
typecheckExpr (ResolvedSyntax.ExprEq lhs rhs) =
  typecheckBinOp TypedSyntax.BinEq <$> typecheckExpr lhs <*> typecheckExpr rhs
typecheckExpr (ResolvedSyntax.ExprNeq lhs rhs) =
  TypedSyntax.ExprUnOp TypedSyntax.UnNot <$>
  (typecheckBinOp TypedSyntax.BinEq <$> typecheckExpr lhs <*> typecheckExpr rhs)
typecheckExpr (ResolvedSyntax.ExprLt lhs rhs) =
  typecheckBinOp TypedSyntax.BinLt <$> typecheckExpr lhs <*> typecheckExpr rhs
typecheckExpr (ResolvedSyntax.ExprLeq lhs rhs) =
  TypedSyntax.ExprUnOp TypedSyntax.UnNot <$>
  (typecheckBinOp TypedSyntax.BinLt <$> typecheckExpr rhs <*> typecheckExpr lhs)
typecheckExpr (ResolvedSyntax.ExprGt lhs rhs) =
  typecheckBinOp TypedSyntax.BinLt <$> typecheckExpr rhs <*> typecheckExpr lhs
typecheckExpr (ResolvedSyntax.ExprGeq lhs rhs) =
  TypedSyntax.ExprUnOp TypedSyntax.UnNot <$>
  (typecheckBinOp TypedSyntax.BinLt <$> typecheckExpr lhs <*> typecheckExpr rhs)
