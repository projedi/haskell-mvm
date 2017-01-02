{-# LANGUAGE FlexibleInstances #-}

module PrettyPrint
  ( prettyPrint
  ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.List as List

import Syntax

prettyPrint :: Program -> String
prettyPrint p =
  unlines
    [ printLibs $ programLibraries p
    , printForeignFunctions $ programForeignFunctions p
    , printFunctions $ programFunctions p
    ]

printLibs :: [String] -> String
printLibs libs = "Libraries: " ++ unwords libs

printForeignFunctions :: IntMap ForeignFunctionDecl -> String
printForeignFunctions funs =
  "Foreign functions: " ++
  IntMap.foldrWithKey
    (\key val rest -> rest ++ "\n" ++ show key ++ ": " ++ printForeignFun val)
    ""
    funs

printForeignFun :: ForeignFunctionDecl -> String
printForeignFun fdecl =
  show (foreignFunDeclRetType fdecl) ++
  " " ++
  foreignFunDeclRealName fdecl ++ " " ++ show (foreignFunDeclParams fdecl)

printFunctions :: IntMap FunctionDef -> String
printFunctions funs =
  "Functions: " ++
  IntMap.foldrWithKey
    (\key val rest ->
        rest ++ "\n" ++ show key ++ ": " ++ prettyPrintFunctionDef 0 val)
    ""
    funs

paren :: String -> String
paren str = "(" ++ str ++ ")"

parenIfNeeded :: Int -> Int -> String -> String
parenIfNeeded prevIndent currentIndent str
  | prevIndent > currentIndent = paren str
  | otherwise = str

prettyPrintExpr :: Int -> Expr -> String
prettyPrintExpr _ (ExprFunctionCall fcall) = prettyPrintSimple fcall
prettyPrintExpr _ (ExprVar varName) = prettyPrintSimple varName
prettyPrintExpr _ (ExprInt n) = show n
prettyPrintExpr _ (ExprFloat n) = show n
prettyPrintExpr _ (ExprString str) = show str
prettyPrintExpr n (ExprNeg e) = parenIfNeeded n 9 $ "-" ++ prettyPrintExpr 9 e
prettyPrintExpr n (ExprPlus el er) =
  parenIfNeeded n 7 $ prettyPrintExpr 7 el ++ " + " ++ prettyPrintExpr 7 er
prettyPrintExpr n (ExprMinus el er) =
  parenIfNeeded n 7 $ prettyPrintExpr 7 el ++ " - " ++ prettyPrintExpr 7 er
prettyPrintExpr n (ExprTimes el er) =
  parenIfNeeded n 8 $ prettyPrintExpr 8 el ++ " * " ++ prettyPrintExpr 8 er
prettyPrintExpr n (ExprDiv el er) =
  parenIfNeeded n 8 $ prettyPrintExpr 8 el ++ " / " ++ prettyPrintExpr 8 er
prettyPrintExpr n (ExprMod el er) =
  parenIfNeeded n 8 $ prettyPrintExpr 8 el ++ " % " ++ prettyPrintExpr 8 er
prettyPrintExpr n (ExprBitAnd el er) =
  parenIfNeeded n 4 $ prettyPrintExpr 4 el ++ " & " ++ prettyPrintExpr 4 er
prettyPrintExpr n (ExprBitOr el er) =
  parenIfNeeded n 2 $ prettyPrintExpr 2 el ++ " | " ++ prettyPrintExpr 2 er
prettyPrintExpr n (ExprBitXor el er) =
  parenIfNeeded n 3 $ prettyPrintExpr 3 el ++ " ^ " ++ prettyPrintExpr 3 er
prettyPrintExpr n (ExprNot e) = parenIfNeeded n 9 $ "!" ++ prettyPrintExpr 9 e
prettyPrintExpr n (ExprAnd el er) =
  parenIfNeeded n 1 $ prettyPrintExpr 1 el ++ " && " ++ prettyPrintExpr 1 er
prettyPrintExpr n (ExprOr el er) =
  parenIfNeeded n 0 $ prettyPrintExpr 0 el ++ " || " ++ prettyPrintExpr 0 er
prettyPrintExpr n (ExprEq el er) =
  parenIfNeeded n 5 $ prettyPrintExpr 5 el ++ " == " ++ prettyPrintExpr 5 er
prettyPrintExpr n (ExprLt el er) =
  parenIfNeeded n 6 $ prettyPrintExpr 6 el ++ " < " ++ prettyPrintExpr 6 er

class PrettyPrintSimple a  where
  prettyPrintSimple :: a -> String

instance PrettyPrintSimple VarID where
  prettyPrintSimple = show

instance PrettyPrintSimple FunID where
  prettyPrintSimple = show

instance PrettyPrintSimple FunctionCall where
  prettyPrintSimple (NativeFunctionCall funname args) =
    prettyPrintSimple funname ++
    paren (List.intercalate ", " (map (prettyPrintExpr 0) args))
  prettyPrintSimple (ForeignFunctionCall funname args) =
    prettyPrintSimple funname ++
    paren (List.intercalate ", " (map (prettyPrintExpr 0) args))
  prettyPrintSimple (PrintCall args) =
    "print" ++ paren (List.intercalate ", " (map (prettyPrintExpr 0) args))

instance PrettyPrintSimple VarDecl where
  prettyPrintSimple (VarDecl vtype name) =
    prettyPrintSimple vtype ++ " " ++ prettyPrintSimple name

instance PrettyPrintSimple VarType where
  prettyPrintSimple = show

instance PrettyPrintSimple (Maybe VarType) where
  prettyPrintSimple (Just vtype) = prettyPrintSimple vtype
  prettyPrintSimple Nothing = "void"

indent :: Int -> String -> String
indent 0 str = str
indent n str = "  " ++ indent (n - 1) str

prettyPrintStatement :: Int -> Statement -> String
prettyPrintStatement n (StatementFunctionCall fcall) =
  indent n (prettyPrintSimple fcall ++ ";")
prettyPrintStatement n (StatementAssign var expr) =
  indent n (prettyPrintSimple var ++ " = " ++ prettyPrintExpr 0 expr ++ ";")
prettyPrintStatement n (StatementReturn Nothing) = indent n "return;"
prettyPrintStatement n (StatementReturn (Just e)) =
  indent n ("return " ++ prettyPrintExpr 0 e ++ ";")
prettyPrintStatement n (StatementWhile e s) =
  indent n ("while (" ++ prettyPrintExpr 0 e ++ ")") ++ prettyPrintBlock n s
prettyPrintStatement n (StatementIfElse e s1 s2) =
  indent n ("if (" ++ prettyPrintExpr 0 e ++ ")\n") ++
  prettyPrintBlock n s1 ++ "\n" ++ indent n "else\n" ++ prettyPrintBlock n s2
prettyPrintStatement n (StatementBlock stmts) = prettyPrintBlock n stmts

prettyPrintBlock :: Int -> Block -> String
prettyPrintBlock n block =
  indent n "{\n" ++
  unlines (map (indent (n + 1) . prettyPrintSimple) $ blockVariables block) ++
  printProgram (n + 1) (blockStatements block) ++ "\n" ++ indent n "}"

prettyPrintFunctionDef :: Int -> FunctionDef -> String
prettyPrintFunctionDef n fdef =
  indent
    n
    (prettyPrintSimple (funDefRetType fdef) ++
     " " ++
     prettyPrintSimple (funDefName fdef) ++
     paren (List.intercalate ", " (map prettyPrintSimple (funDefParams fdef)))) ++
  " [" ++
  List.intercalate ", " (map prettyPrintSimple (funDefCaptures fdef)) ++
  "]" ++ "\n" ++ prettyPrintBlock n (funDefBody fdef)
  where
    funDefCaptures = map VarID . IntSet.elems . varAccess . funDefAccesses

printProgram :: Int -> [Statement] -> String
printProgram n stmts =
  List.intercalate "\n" (map (prettyPrintStatement n) stmts)
