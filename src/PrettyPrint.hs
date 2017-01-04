{-# LANGUAGE FlexibleInstances #-}

module PrettyPrint
  ( prettyPrint
  ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
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

binOpPrec :: BinOp -> Int
binOpPrec BinPlus = 7
binOpPrec BinMinus = 7
binOpPrec BinTimes = 8
binOpPrec BinDiv = 8
binOpPrec BinMod = 8
binOpPrec BinBitAnd = 4
binOpPrec BinBitOr = 2
binOpPrec BinBitXor = 3
binOpPrec BinAnd = 1
binOpPrec BinOr = 0
binOpPrec BinEq = 5
binOpPrec BinLt = 6

prettyPrintBinOp :: BinOp -> String
prettyPrintBinOp BinPlus = "+"
prettyPrintBinOp BinMinus = "-"
prettyPrintBinOp BinTimes = "*"
prettyPrintBinOp BinDiv = "/"
prettyPrintBinOp BinMod = "%"
prettyPrintBinOp BinBitAnd = "&"
prettyPrintBinOp BinBitOr = "|"
prettyPrintBinOp BinBitXor = "^"
prettyPrintBinOp BinAnd = "&&"
prettyPrintBinOp BinOr = "||"
prettyPrintBinOp BinEq = "=="
prettyPrintBinOp BinLt = "<"

unOpPrec :: UnOp -> Int
unOpPrec UnNeg = 9
unOpPrec UnNot = 9
unOpPrec UnIntToFloat = 9

prettyPrintUnOp :: UnOp -> String
prettyPrintUnOp UnNeg = "-"
prettyPrintUnOp UnNot = "!"
prettyPrintUnOp UnIntToFloat = "(double)"

prettyPrintExpr :: Int -> Expr -> String
prettyPrintExpr _ (ExprFunctionCall fcall) = prettyPrintSimple fcall
prettyPrintExpr _ (ExprVar _ varName) = prettyPrintSimple varName
prettyPrintExpr _ (ExprInt n) = show n
prettyPrintExpr _ (ExprFloat n) = show n
prettyPrintExpr _ (ExprString str) = show str
prettyPrintExpr n (ExprUnOp op e) =
  parenIfNeeded n (unOpPrec op) $
  prettyPrintUnOp op ++ prettyPrintExpr (unOpPrec op) e
prettyPrintExpr n (ExprBinOp op el er) =
  parenIfNeeded n (binOpPrec op) $
  prettyPrintExpr (binOpPrec op) el ++
  " " ++ prettyPrintBinOp op ++ " " ++ prettyPrintExpr (binOpPrec op) er
prettyPrintExpr _ _ = undefined -- TODO: Remove when pattern synonyms have COMPLETE pragma.

class PrettyPrintSimple a  where
  prettyPrintSimple :: a -> String

instance PrettyPrintSimple VarID where
  prettyPrintSimple = show

instance PrettyPrintSimple FunID where
  prettyPrintSimple = show

instance PrettyPrintSimple FunctionCall where
  prettyPrintSimple NativeFunctionCall{ nativeFunCallName = funname, nativeFunCallArgs = args, nativeFunCallCaptures = captures} =
    prettyPrintSimple funname ++
    paren (List.intercalate ", " (map (prettyPrintExpr 0) args)) ++
    "[" ++ (List.intercalate ", " (map prettyPrintSimple captures)) ++ "]"
  prettyPrintSimple ForeignFunctionCall{ foreignFunCallName = funname, foreignFunCallArgs = args} =
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

printProgram :: Int -> [Statement] -> String
printProgram n stmts =
  List.intercalate "\n" (map (prettyPrintStatement n) stmts)
