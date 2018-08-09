module SyntaxSimplifier
  ( simplify
  ) where

import Control.Monad
import Control.Monad.Reader (ReaderT, runReaderT)
import qualified Control.Monad.Reader as Reader
import Control.Monad.State (State, evalState)
import qualified Control.Monad.State as State
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import qualified SimplifiedSyntax
import qualified TypedSyntax

data Env = Env
  { vars :: IntMap SimplifiedSyntax.VarType
  , funs :: IntMap TypedSyntax.FunctionDef
  , foreignFuns :: IntMap SimplifiedSyntax.ForeignFunctionDecl
  , strings :: IntMap String
  , lastFunID :: SimplifiedSyntax.FunID
  , lastVarID :: SimplifiedSyntax.VarID
  , lastStringID :: SimplifiedSyntax.StringID
  , printfFunID :: SimplifiedSyntax.FunID
  }

type Simplifier = State Env

simplify :: TypedSyntax.Program -> SimplifiedSyntax.Program
simplify p =
  evalState (simplifyProgram p) $
  Env
    { vars = TypedSyntax.programVariables p
    , funs = TypedSyntax.programFunctions p
    , foreignFuns =
        IntMap.map simplifyForeignFunctionDecl $
        TypedSyntax.programForeignFunctions p
    , strings = TypedSyntax.programStrings p
    , lastFunID = TypedSyntax.programLastFunID p
    , lastVarID = TypedSyntax.programLastVarID p
    , lastStringID = TypedSyntax.programLastStringID p
    , printfFunID = SimplifiedSyntax.FunID (-1)
    }

simplifyProgram :: TypedSyntax.Program -> Simplifier SimplifiedSyntax.Program
simplifyProgram p = do
  introducePrintf
  fs <- mapM simplifyFunctionDef =<< State.gets funs
  finalEnv <- State.get
  pure $
    SimplifiedSyntax.Program
      { SimplifiedSyntax.programFunctions = fs
      , SimplifiedSyntax.programLibraries = TypedSyntax.programLibraries p
      , SimplifiedSyntax.programForeignFunctions = foreignFuns finalEnv
      , SimplifiedSyntax.programStrings = strings finalEnv
      , SimplifiedSyntax.programVariables = vars finalEnv
      , SimplifiedSyntax.programLastFunID = lastFunID finalEnv
      , SimplifiedSyntax.programLastVarID = lastVarID finalEnv
      , SimplifiedSyntax.programLastStringID = lastStringID finalEnv
      }

printfDecl :: SimplifiedSyntax.FunID -> SimplifiedSyntax.ForeignFunctionDecl
printfDecl fid =
  SimplifiedSyntax.ForeignFunctionDecl
    { SimplifiedSyntax.foreignFunDeclRetType = Nothing
    , SimplifiedSyntax.foreignFunDeclName = fid
    , SimplifiedSyntax.foreignFunDeclRealName = "printf"
    , SimplifiedSyntax.foreignFunDeclParams = [SimplifiedSyntax.VarTypeString]
    , SimplifiedSyntax.foreignFunDeclHasVarArgs = True
    }

introducePrintf :: Simplifier ()
introducePrintf = do
  newFunID@(SimplifiedSyntax.FunID fid) <- State.gets (inc . lastFunID)
  State.modify $ \env ->
    env
      { lastFunID = newFunID
      , printfFunID = newFunID
      , foreignFuns = IntMap.insert fid (printfDecl newFunID) $ foreignFuns env
      }
  where
    inc :: SimplifiedSyntax.FunID -> SimplifiedSyntax.FunID
    inc (SimplifiedSyntax.FunID vid) = SimplifiedSyntax.FunID (vid + 1)

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
          (\(SimplifiedSyntax.VarDecl _ pid, SimplifiedSyntax.VarID vid) ->
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

newtype ConstEnv = ConstEnv
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

generatePrintfArgs ::
     [SimplifiedSyntax.Expr] -> FunctionBodySimplifier [SimplifiedSyntax.Expr]
generatePrintfArgs args = do
  newStringID@(SimplifiedSyntax.StringID sid) <- State.gets (inc . lastStringID)
  State.modify $ \env ->
    env
      { lastStringID = newStringID
      , strings = IntMap.insert sid printfFormatString $ strings env
      }
  pure $
    (SimplifiedSyntax.ExprConst $ SimplifiedSyntax.ImmediateString newStringID) :
    args
  where
    inc :: SimplifiedSyntax.StringID -> SimplifiedSyntax.StringID
    inc (SimplifiedSyntax.StringID sid) = SimplifiedSyntax.StringID (sid + 1)
    printfFormatString =
      concatMap (formatStringArg . SimplifiedSyntax.exprType) args
    formatStringArg SimplifiedSyntax.VarTypeInt = "%ld"
    formatStringArg SimplifiedSyntax.VarTypeFloat = "%g"
    formatStringArg SimplifiedSyntax.VarTypeString = "%s"
    formatStringArg (SimplifiedSyntax.VarTypePtr _) = error "Type error"

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
simplifyFunctionCall (TypedSyntax.PrintCall es) = do
  args <- generatePrintfArgs =<< mapM simplifyExpr es
  printfName <- State.gets printfFunID
  pure $
    SimplifiedSyntax.ForeignFunctionCall
      { SimplifiedSyntax.foreignFunCallName = printfName
      , SimplifiedSyntax.foreignFunCallRetType = Nothing
      , SimplifiedSyntax.foreignFunCallArgs = args
      }

simplifyExpr :: TypedSyntax.Expr -> FunctionBodySimplifier SimplifiedSyntax.Expr
simplifyExpr (TypedSyntax.ExprFunctionCall fcall) =
  SimplifiedSyntax.ExprFunctionCall <$> simplifyFunctionCall fcall
simplifyExpr (TypedSyntax.ExprVar t v) =
  tryAsCapture
    v
    (pure . SimplifiedSyntax.ExprDereference t)
    (pure . SimplifiedSyntax.ExprVar t)
simplifyExpr (TypedSyntax.ExprConst c) = pure $ SimplifiedSyntax.ExprConst c
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
