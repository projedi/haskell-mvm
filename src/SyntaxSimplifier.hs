module SyntaxSimplifier
  ( simplify
  ) where

import Control.Monad
import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Reader as Reader
import Control.Monad.State (State, runState)
import qualified Control.Monad.State as State
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import qualified SimplifiedSyntax
import qualified TypedSyntax

data Env = Env
  { vars :: IntMap SimplifiedSyntax.VarType
  , funs :: IntMap TypedSyntax.FunctionDef
  , foreignFuns :: IntMap SimplifiedSyntax.ForeignFunctionDecl
  , lastFunID :: SimplifiedSyntax.FunID
  , lastVarID :: SimplifiedSyntax.VarID
  , lastConstID :: SimplifiedSyntax.ConstID
  }

type Simplifier = State Env

simplify :: TypedSyntax.Program -> SimplifiedSyntax.Program
simplify p =
  SimplifiedSyntax.Program
    { SimplifiedSyntax.programFunctions = fs
    , SimplifiedSyntax.programLibraries = TypedSyntax.programLibraries p
    , SimplifiedSyntax.programForeignFunctions = foreignFuns finalEnv
    , SimplifiedSyntax.programConstants = TypedSyntax.programConstants p
    , SimplifiedSyntax.programVariables = vars finalEnv
    , SimplifiedSyntax.programLastFunID = lastFunID finalEnv
    , SimplifiedSyntax.programLastVarID = lastVarID finalEnv
    , SimplifiedSyntax.programLastConstID = lastConstID finalEnv
    }
  where
    (fs, finalEnv) =
      runState (mapM simplifyFunctionDef (TypedSyntax.programFunctions p)) $
      Env
        { vars = TypedSyntax.programVariables p
        , funs = TypedSyntax.programFunctions p
        , foreignFuns =
            IntMap.map simplifyForeignFunctionDecl $
            TypedSyntax.programForeignFunctions p
        , lastFunID = TypedSyntax.programLastFunID p
        , lastVarID = TypedSyntax.programLastVarID p
        , lastConstID = TypedSyntax.programLastConstID p
        }

simplifyForeignFunctionDecl ::
     TypedSyntax.ForeignFunctionDecl -> SimplifiedSyntax.ForeignFunctionDecl
simplifyForeignFunctionDecl ffdecl =
  SimplifiedSyntax.ForeignFunctionDecl
    { SimplifiedSyntax.foreignFunDeclRetType =
        TypedSyntax.foreignFunDeclRetType ffdecl
    , SimplifiedSyntax.foreignFunDeclName =
        TypedSyntax.foreignFunDeclName ffdecl
    , SimplifiedSyntax.foreignFunDeclRealName =
        TypedSyntax.foreignFunDeclRealName ffdecl
    , SimplifiedSyntax.foreignFunDeclParams =
        TypedSyntax.foreignFunDeclParams ffdecl
    , SimplifiedSyntax.foreignFunDeclHasVarArgs = False
    }

simplifyFunctionDef ::
     TypedSyntax.FunctionDef -> Simplifier SimplifiedSyntax.FunctionDef
simplifyFunctionDef fdef = do
  vs <- State.gets vars
  resultingCaptures <- convertCaptures (originalCaptures vs)
  let newVs = newVariables vs $ map fst resultingCaptures
      extraParams = map fst resultingCaptures
      resultingMapping =
        IntMap.fromList $
        map
          (\((SimplifiedSyntax.VarDecl _ pid), (SimplifiedSyntax.VarID vid)) ->
             (vid, pid))
          resultingCaptures
  State.modify $ \env -> env {vars = newVs}
  body <-
    runReaderT (simplifyBlock $ TypedSyntax.funDefBody fdef) $
    ConstEnv {capturesMapping = resultingMapping}
  pure $
    SimplifiedSyntax.FunctionDef
      { SimplifiedSyntax.funDefRetType = TypedSyntax.funDefRetType fdef
      , SimplifiedSyntax.funDefName = TypedSyntax.funDefName fdef
      , SimplifiedSyntax.funDefParams =
          extraParams ++ TypedSyntax.funDefParams fdef
      , SimplifiedSyntax.funDefBody = body
      }
  where
    originalCaptures ::
         IntMap SimplifiedSyntax.VarType
      -> [(SimplifiedSyntax.VarType, SimplifiedSyntax.VarID)]
    originalCaptures vs =
      map
        (\(SimplifiedSyntax.VarID vid) ->
           (vs IntMap.! vid, SimplifiedSyntax.VarID vid))
        (TypedSyntax.funDefCaptures fdef)
    convertCaptures =
      mapM $ \(vt, vid) -> do
        newVarID <- State.gets (inc . lastVarID)
        State.modify $ \env -> env {lastVarID = newVarID}
        pure
          ( SimplifiedSyntax.VarDecl (SimplifiedSyntax.VarTypePtr vt) newVarID
          , vid)
    newVariables ::
         IntMap SimplifiedSyntax.VarType
      -> [SimplifiedSyntax.VarDecl]
      -> IntMap SimplifiedSyntax.VarType
    newVariables originalVariables captures =
      originalVariables `IntMap.union`
      IntMap.fromList
        (map
           (\(SimplifiedSyntax.VarDecl vt (SimplifiedSyntax.VarID vid)) ->
              (vid, vt))
           captures)
    inc :: SimplifiedSyntax.VarID -> SimplifiedSyntax.VarID
    inc (SimplifiedSyntax.VarID vid) = SimplifiedSyntax.VarID (vid + 1)

data ConstEnv = ConstEnv
  { capturesMapping :: IntMap SimplifiedSyntax.VarID
  }

type FunctionBodySimplifier = ReaderT ConstEnv Simplifier

tryAsCapture ::
     TypedSyntax.VarID
  -> (TypedSyntax.VarID -> FunctionBodySimplifier a)
  -> (TypedSyntax.VarID -> FunctionBodySimplifier a)
  -> FunctionBodySimplifier a
tryAsCapture v@(TypedSyntax.VarID vid) asCapture asLocal = do
  capture <- Reader.asks (IntMap.lookup vid . capturesMapping)
  case capture of
    Just p -> asCapture p
    Nothing -> asLocal v

simplifyBlock ::
     TypedSyntax.Block -> FunctionBodySimplifier SimplifiedSyntax.Block
simplifyBlock block = do
  stmts <- mapM simplifyStatement (TypedSyntax.blockStatements block)
  pure $ SimplifiedSyntax.Block {SimplifiedSyntax.blockStatements = stmts}

simplifyStatement ::
     TypedSyntax.Statement -> FunctionBodySimplifier SimplifiedSyntax.Statement
simplifyStatement (TypedSyntax.StatementBlock block) =
  SimplifiedSyntax.StatementBlock <$> simplifyBlock block
simplifyStatement (TypedSyntax.StatementVarAlloc v) =
  pure $ SimplifiedSyntax.StatementVarAlloc v
simplifyStatement (TypedSyntax.StatementFunctionCall fcall) =
  SimplifiedSyntax.StatementFunctionCall <$> simplifyFunctionCall fcall
simplifyStatement (TypedSyntax.StatementWhile expr block) =
  SimplifiedSyntax.StatementWhile <$> simplifyExpr expr <*> simplifyBlock block
simplifyStatement (TypedSyntax.StatementAssign v expr) =
  tryAsCapture
    v
    (pure . SimplifiedSyntax.StatementAssignToPtr)
    (pure . SimplifiedSyntax.StatementAssign) <*>
  simplifyExpr expr
simplifyStatement (TypedSyntax.StatementIfElse expr blockTrue blockFalse) =
  SimplifiedSyntax.StatementIfElse <$> simplifyExpr expr <*>
  simplifyBlock blockTrue <*>
  simplifyBlock blockFalse
simplifyStatement (TypedSyntax.StatementReturn Nothing) =
  pure $ SimplifiedSyntax.StatementReturn Nothing
simplifyStatement (TypedSyntax.StatementReturn (Just expr)) =
  SimplifiedSyntax.StatementReturn . Just <$> simplifyExpr expr

simplifyFunctionCall ::
     TypedSyntax.FunctionCall
  -> FunctionBodySimplifier SimplifiedSyntax.FunctionCall
simplifyFunctionCall fcall@TypedSyntax.NativeFunctionCall {TypedSyntax.nativeFunCallName = TypedSyntax.FunID fid} = do
  captures <- State.gets (TypedSyntax.funDefCaptures . (IntMap.! fid) . funs)
  captureArgs <-
    forM captures $ \c@(TypedSyntax.VarID cid) -> do
      ct <- State.gets ((IntMap.! cid) . vars)
      tryAsCapture
        c
        (pure . SimplifiedSyntax.ExprVar (TypedSyntax.VarTypePtr ct))
        (pure . SimplifiedSyntax.ExprAddressOf (TypedSyntax.VarTypePtr ct))
  args <- mapM simplifyExpr (TypedSyntax.nativeFunCallArgs fcall)
  pure $
    SimplifiedSyntax.NativeFunctionCall
      { SimplifiedSyntax.nativeFunCallName = TypedSyntax.nativeFunCallName fcall
      , SimplifiedSyntax.nativeFunCallRetType =
          TypedSyntax.nativeFunCallRetType fcall
      , SimplifiedSyntax.nativeFunCallArgs = captureArgs ++ args
      }
simplifyFunctionCall fcall@TypedSyntax.ForeignFunctionCall {} = do
  args <- mapM simplifyExpr (TypedSyntax.foreignFunCallArgs fcall)
  pure $
    SimplifiedSyntax.ForeignFunctionCall
      { SimplifiedSyntax.foreignFunCallName =
          TypedSyntax.foreignFunCallName fcall
      , SimplifiedSyntax.foreignFunCallRetType =
          TypedSyntax.foreignFunCallRetType fcall
      , SimplifiedSyntax.foreignFunCallArgs = args
      }
simplifyFunctionCall (TypedSyntax.PrintCall es) =
  SimplifiedSyntax.PrintCall <$> mapM simplifyExpr es

simplifyExpr :: TypedSyntax.Expr -> FunctionBodySimplifier SimplifiedSyntax.Expr
simplifyExpr (TypedSyntax.ExprFunctionCall fcall) =
  SimplifiedSyntax.ExprFunctionCall <$> simplifyFunctionCall fcall
simplifyExpr (TypedSyntax.ExprVar t v) =
  tryAsCapture
    v
    (pure . SimplifiedSyntax.ExprDereference t)
    (pure . SimplifiedSyntax.ExprVar t)
simplifyExpr (TypedSyntax.ExprConst t c) = pure $ SimplifiedSyntax.ExprConst t c
simplifyExpr (TypedSyntax.ExprBinOp op lhs rhs) =
  simplifyBinOp op <$> simplifyExpr lhs <*> simplifyExpr rhs
simplifyExpr (TypedSyntax.ExprUnOp op arg) =
  simplifyUnOp op <$> simplifyExpr arg

simplifyBinOp ::
     TypedSyntax.BinOp
  -> SimplifiedSyntax.Expr
  -> SimplifiedSyntax.Expr
  -> SimplifiedSyntax.Expr
simplifyBinOp TypedSyntax.BinPlus =
  SimplifiedSyntax.ExprBinOp SimplifiedSyntax.BinPlus
simplifyBinOp TypedSyntax.BinMinus =
  SimplifiedSyntax.ExprBinOp SimplifiedSyntax.BinMinus
simplifyBinOp TypedSyntax.BinTimes =
  SimplifiedSyntax.ExprBinOp SimplifiedSyntax.BinTimes
simplifyBinOp TypedSyntax.BinDiv =
  SimplifiedSyntax.ExprBinOp SimplifiedSyntax.BinDiv
simplifyBinOp TypedSyntax.BinMod =
  SimplifiedSyntax.ExprBinOp SimplifiedSyntax.BinMod
simplifyBinOp TypedSyntax.BinBitAnd =
  SimplifiedSyntax.ExprBinOp SimplifiedSyntax.BinBitAnd
simplifyBinOp TypedSyntax.BinBitOr =
  SimplifiedSyntax.ExprBinOp SimplifiedSyntax.BinBitOr
simplifyBinOp TypedSyntax.BinBitXor =
  SimplifiedSyntax.ExprBinOp SimplifiedSyntax.BinBitXor
simplifyBinOp TypedSyntax.BinAnd =
  SimplifiedSyntax.ExprBinOp SimplifiedSyntax.BinAnd
simplifyBinOp TypedSyntax.BinOr =
  SimplifiedSyntax.ExprBinOp SimplifiedSyntax.BinOr
simplifyBinOp TypedSyntax.BinEq =
  SimplifiedSyntax.ExprBinOp SimplifiedSyntax.BinEq
simplifyBinOp TypedSyntax.BinLt =
  SimplifiedSyntax.ExprBinOp SimplifiedSyntax.BinLt

simplifyUnOp ::
     TypedSyntax.UnOp -> SimplifiedSyntax.Expr -> SimplifiedSyntax.Expr
simplifyUnOp TypedSyntax.UnNeg =
  SimplifiedSyntax.ExprUnOp SimplifiedSyntax.UnNeg
simplifyUnOp TypedSyntax.UnNot =
  SimplifiedSyntax.ExprUnOp SimplifiedSyntax.UnNot
simplifyUnOp TypedSyntax.UnIntToFloat =
  SimplifiedSyntax.ExprUnOp SimplifiedSyntax.UnIntToFloat
