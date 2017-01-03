{-# LANGUAGE ViewPatterns #-}
module TypeChecker
  ( typeCheck
  ) where

import Control.Monad
import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Reader as Reader
import Control.Monad.State (State, runState)
import qualified Control.Monad.State as State
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import qualified ResolvedSyntax
import qualified Syntax

typeCheck :: ResolvedSyntax.Program -> Syntax.Program
typeCheck p =
  Syntax.Program
  { Syntax.programFunctions = fs
  , Syntax.programLibraries = ResolvedSyntax.programLibraries p
  , Syntax.programForeignFunctions = envForeignFunctions finalEnv
  , Syntax.programLastFunID = envLastFunID finalEnv
  , Syntax.programLastVarID = envLastVarID finalEnv
  }
  where
    startEnv =
      Env
      { envFuns = ResolvedSyntax.programFunctions p
      , envForeignFunctions = ResolvedSyntax.programForeignFunctions p
      , envLastFunID = ResolvedSyntax.programLastFunID p
      , envLastVarID = ResolvedSyntax.programLastVarID p
      , envVars = IntMap.empty
      }
    (fs, finalEnv) = runTypeChecker (ResolvedSyntax.programFunctions p) startEnv

data ConstEnv = ConstEnv
  { constEnvRetType :: Maybe Syntax.VarType
  }

data Env = Env
  { envFuns :: IntMap ResolvedSyntax.FunctionDef
  , envForeignFunctions :: IntMap ResolvedSyntax.ForeignFunctionDecl
  , envLastFunID :: Syntax.FunID
  , envLastVarID :: Syntax.VarID
  , envVars :: IntMap Syntax.VarType
  }

type TypeChecker = ReaderT ConstEnv (State Env)

runTypeChecker :: IntMap ResolvedSyntax.FunctionDef
               -> Env
               -> (IntMap Syntax.FunctionDef, Env)
runTypeChecker fs = runState (sequence $ fmap go fs)
  where
    constEnv f = ConstEnv
      { constEnvRetType = ResolvedSyntax.funDefRetType f
      }
    go f = (flip runReaderT) (constEnv f) $ do
      forM_ (ResolvedSyntax.funDefParams f) introduceVariable
      body <- typecheckBlock $ ResolvedSyntax.funDefBody f
      pure
        Syntax.FunctionDef
        { Syntax.funDefRetType = ResolvedSyntax.funDefRetType f
        , Syntax.funDefName = ResolvedSyntax.funDefName f
        , Syntax.funDefParams = ResolvedSyntax.funDefParams f
        , Syntax.funDefAccesses = ResolvedSyntax.funDefAccesses f
        , Syntax.funDefBody = body
        }

introduceVariable :: ResolvedSyntax.VarDecl -> TypeChecker ()
introduceVariable (ResolvedSyntax.VarDecl t (Syntax.VarID vid)) =
  State.modify $ \env -> env { envVars = IntMap.insert vid t (envVars env) }

typecheckBlock :: ResolvedSyntax.Block -> TypeChecker Syntax.Block
typecheckBlock block = do
  forM_ (ResolvedSyntax.blockVariables block) introduceVariable
  stmts <- mapM typecheckStatement (ResolvedSyntax.blockStatements block)
  pure
    Syntax.Block
    { Syntax.blockVariables = ResolvedSyntax.blockVariables block
    , Syntax.blockStatements = stmts
    }

noopBlock :: Syntax.Block
noopBlock =
  Syntax.Block
  { Syntax.blockVariables = []
  , Syntax.blockStatements = []
  }

typecheckStatement :: ResolvedSyntax.Statement -> TypeChecker Syntax.Statement
typecheckStatement (ResolvedSyntax.StatementBlock block) =
  Syntax.StatementBlock <$> typecheckBlock block
typecheckStatement (ResolvedSyntax.StatementFunctionCall fcall) =
  Syntax.StatementFunctionCall <$> typecheckFunctionCall fcall
typecheckStatement (ResolvedSyntax.StatementWhile e block) =
  Syntax.StatementWhile <$> typecheckExpr e <*> typecheckBlock block
typecheckStatement (ResolvedSyntax.StatementAssign v e) =
  Syntax.StatementAssign v <$> typecheckExpr e
typecheckStatement (ResolvedSyntax.StatementAssignPlus v e) =
  Syntax.StatementAssign v <$> typecheckExpr e'
  where
    e' = ResolvedSyntax.ExprPlus (ResolvedSyntax.ExprVar v) e
typecheckStatement (ResolvedSyntax.StatementAssignMinus v e) =
  Syntax.StatementAssign v <$> typecheckExpr e'
  where
    e' = ResolvedSyntax.ExprMinus (ResolvedSyntax.ExprVar v) e
typecheckStatement (ResolvedSyntax.StatementIfElse e bt bf) =
  Syntax.StatementIfElse <$> typecheckExpr e <*> typecheckBlock bt <*>
  typecheckBlock bf
typecheckStatement (ResolvedSyntax.StatementIf e bt) =
  Syntax.StatementIfElse <$> typecheckExpr e <*> typecheckBlock bt <*> pure noopBlock
typecheckStatement (ResolvedSyntax.StatementReturn Nothing) = do
  Nothing <- constEnvRetType <$> Reader.ask
  pure $ Syntax.StatementReturn Nothing
typecheckStatement (ResolvedSyntax.StatementReturn (Just e)) = do
  Just expectedType <- constEnvRetType <$> Reader.ask
  (Syntax.StatementReturn . Just . convertExprTo expectedType) <$> typecheckExpr e

convertExprTo :: Syntax.VarType -> Syntax.Expr -> Syntax.Expr
convertExprTo Syntax.VarTypeInt e@(Syntax.exprType -> Syntax.VarTypeInt) = e
convertExprTo Syntax.VarTypeFloat e@(Syntax.exprType -> Syntax.VarTypeFloat) = e
convertExprTo Syntax.VarTypeString e@(Syntax.exprType -> Syntax.VarTypeString) = e
convertExprTo Syntax.VarTypeFloat e@(Syntax.exprType -> Syntax.VarTypeInt) =
  Syntax.ExprUnOp Syntax.UnIntToFloat e
convertExprTo _ _ = error "Type mismatch"

convertArgsTo :: [Syntax.VarType] -> [Syntax.Expr] -> [Syntax.Expr]
convertArgsTo [] [] = []
convertArgsTo (t:ts) (e:es) = convertExprTo t e : convertArgsTo ts es
convertArgsTo _ _ = error "Type mismatch"

typecheckFunctionCall :: ResolvedSyntax.FunctionCall -> TypeChecker Syntax.FunctionCall
typecheckFunctionCall (ResolvedSyntax.NativeFunctionCall (Syntax.FunID fid) args) = do
  Just fdef <- (IntMap.lookup fid . envFuns) <$> State.get
  args' <- convertArgsTo (map (\(ResolvedSyntax.VarDecl t _) -> t) $ ResolvedSyntax.funDefParams fdef) <$> mapM typecheckExpr args
  pure $ Syntax.NativeFunctionCall
    { Syntax.nativeFunCallName = ResolvedSyntax.funDefName fdef
    , Syntax.nativeFunCallRetType = ResolvedSyntax.funDefRetType fdef
    , Syntax.nativeFunCallArgs = args'
    }
typecheckFunctionCall (ResolvedSyntax.ForeignFunctionCall (Syntax.FunID fid) args) = do
  Just fdef <- (IntMap.lookup fid . envForeignFunctions) <$> State.get
  args' <- convertArgsTo (ResolvedSyntax.foreignFunDeclParams fdef) <$> mapM typecheckExpr args
  pure $ Syntax.ForeignFunctionCall
    { Syntax.foreignFunCallName = ResolvedSyntax.foreignFunDeclName fdef
    , Syntax.foreignFunCallRetType = ResolvedSyntax.foreignFunDeclRetType fdef
    , Syntax.foreignFunCallArgs = args'
    }
typecheckFunctionCall (ResolvedSyntax.PrintCall args) =
  Syntax.PrintCall <$> mapM typecheckExpr args

selectBinOpType :: Syntax.VarType -> Syntax.VarType -> Syntax.VarType
selectBinOpType Syntax.VarTypeInt Syntax.VarTypeInt = Syntax.VarTypeInt
selectBinOpType Syntax.VarTypeFloat Syntax.VarTypeFloat = Syntax.VarTypeFloat
selectBinOpType Syntax.VarTypeString Syntax.VarTypeString = Syntax.VarTypeString
selectBinOpType Syntax.VarTypeInt Syntax.VarTypeFloat = Syntax.VarTypeFloat
selectBinOpType Syntax.VarTypeFloat Syntax.VarTypeInt = Syntax.VarTypeFloat
selectBinOpType _ _ = error "Type mismatch"

typecheckBinOp :: Syntax.BinOp -> Syntax.Expr -> Syntax.Expr -> Syntax.Expr
typecheckBinOp op lhs rhs = Syntax.ExprBinOp op (convertExprTo t lhs) (convertExprTo t rhs)
  where
    t = selectBinOpType (Syntax.exprType lhs) (Syntax.exprType rhs)

typecheckExpr :: ResolvedSyntax.Expr -> TypeChecker Syntax.Expr
typecheckExpr (ResolvedSyntax.ExprFunctionCall fcall) =
  Syntax.ExprFunctionCall <$> typecheckFunctionCall fcall
typecheckExpr (ResolvedSyntax.ExprVar (Syntax.VarID v)) = do
  Just t <- (IntMap.lookup v . envVars) <$> State.get
  pure $ Syntax.ExprVar t (Syntax.VarID v)
typecheckExpr (ResolvedSyntax.ExprInt i) = pure $ Syntax.ExprInt i
typecheckExpr (ResolvedSyntax.ExprFloat f) = pure $ Syntax.ExprFloat f
typecheckExpr (ResolvedSyntax.ExprString s) = pure $ Syntax.ExprString s
typecheckExpr (ResolvedSyntax.ExprNeg e) =
  Syntax.ExprUnOp Syntax.UnNeg <$> typecheckExpr e
typecheckExpr (ResolvedSyntax.ExprPlus lhs rhs) =
  typecheckBinOp Syntax.BinPlus <$> typecheckExpr lhs <*> typecheckExpr rhs
typecheckExpr (ResolvedSyntax.ExprMinus lhs rhs) =
  typecheckBinOp Syntax.BinMinus <$> typecheckExpr lhs <*> typecheckExpr rhs
typecheckExpr (ResolvedSyntax.ExprTimes lhs rhs) =
  typecheckBinOp Syntax.BinTimes <$> typecheckExpr lhs <*> typecheckExpr rhs
typecheckExpr (ResolvedSyntax.ExprDiv lhs rhs) =
  typecheckBinOp Syntax.BinDiv <$> typecheckExpr lhs <*> typecheckExpr rhs
typecheckExpr (ResolvedSyntax.ExprMod lhs rhs) =
  typecheckBinOp Syntax.BinMod <$> typecheckExpr lhs <*> typecheckExpr rhs
typecheckExpr (ResolvedSyntax.ExprBitAnd lhs rhs) =
  typecheckBinOp Syntax.BinBitAnd <$> typecheckExpr lhs <*> typecheckExpr rhs
typecheckExpr (ResolvedSyntax.ExprBitOr lhs rhs) =
  typecheckBinOp Syntax.BinBitOr <$> typecheckExpr lhs <*> typecheckExpr rhs
typecheckExpr (ResolvedSyntax.ExprBitXor lhs rhs) =
  typecheckBinOp Syntax.BinBitXor <$> typecheckExpr lhs <*> typecheckExpr rhs
typecheckExpr (ResolvedSyntax.ExprNot e) =
  Syntax.ExprUnOp Syntax.UnNot <$> typecheckExpr e
typecheckExpr (ResolvedSyntax.ExprAnd lhs rhs) =
  typecheckBinOp Syntax.BinAnd <$> typecheckExpr lhs <*> typecheckExpr rhs
typecheckExpr (ResolvedSyntax.ExprOr lhs rhs) =
  typecheckBinOp Syntax.BinOr <$> typecheckExpr lhs <*> typecheckExpr rhs
typecheckExpr (ResolvedSyntax.ExprEq lhs rhs) =
  typecheckBinOp Syntax.BinEq <$> typecheckExpr lhs <*> typecheckExpr rhs
typecheckExpr (ResolvedSyntax.ExprNeq lhs rhs) =
  Syntax.ExprUnOp Syntax.UnNot <$>
  (typecheckBinOp Syntax.BinEq <$> typecheckExpr lhs <*> typecheckExpr rhs)
typecheckExpr (ResolvedSyntax.ExprLt lhs rhs) =
  typecheckBinOp Syntax.BinLt <$> typecheckExpr lhs <*> typecheckExpr rhs
typecheckExpr (ResolvedSyntax.ExprLeq lhs rhs) =
  Syntax.ExprUnOp Syntax.UnNot <$>
  (typecheckBinOp Syntax.BinLt <$> typecheckExpr rhs <*> typecheckExpr lhs)
typecheckExpr (ResolvedSyntax.ExprGt lhs rhs) =
  typecheckBinOp Syntax.BinLt <$> typecheckExpr rhs <*> typecheckExpr lhs
typecheckExpr (ResolvedSyntax.ExprGeq lhs rhs) =
  Syntax.ExprUnOp Syntax.UnNot <$>
  (typecheckBinOp Syntax.BinLt <$> typecheckExpr lhs <*> typecheckExpr rhs)
