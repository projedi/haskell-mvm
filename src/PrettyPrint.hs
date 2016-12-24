{-# LANGUAGE FlexibleInstances #-}

module PrettyPrint
  ( prettyPrint
  ) where

import qualified Data.List as List

import Syntax

prettyPrint :: Program -> String
prettyPrint (Program stmts libs) = printLibs libs ++ printProgram 0 stmts

printLibs :: [String] -> String
printLibs = unlines . map ("#link " ++)

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
prettyPrintExpr n (ExprNeq el er) =
  parenIfNeeded n 5 $ prettyPrintExpr 5 el ++ " != " ++ prettyPrintExpr 5 er
prettyPrintExpr n (ExprLt el er) =
  parenIfNeeded n 6 $ prettyPrintExpr 6 el ++ " < " ++ prettyPrintExpr 6 er
prettyPrintExpr n (ExprLeq el er) =
  parenIfNeeded n 6 $ prettyPrintExpr 6 el ++ " <= " ++ prettyPrintExpr 6 er
prettyPrintExpr n (ExprGt el er) =
  parenIfNeeded n 6 $ prettyPrintExpr 6 el ++ " > " ++ prettyPrintExpr 6 er
prettyPrintExpr n (ExprGeq el er) =
  parenIfNeeded n 6 $ prettyPrintExpr 6 el ++ " >= " ++ prettyPrintExpr 6 er

class PrettyPrintSimple a  where
  prettyPrintSimple :: a -> String

instance PrettyPrintSimple VarName where
  prettyPrintSimple (VarName var) = var

instance PrettyPrintSimple FunctionName where
  prettyPrintSimple (FunctionName var) = var

instance PrettyPrintSimple FunctionCall where
  prettyPrintSimple (FunctionCall funname args) =
    prettyPrintSimple funname ++
    paren (List.intercalate ", " (map (prettyPrintExpr 0) args))

instance PrettyPrintSimple FunctionDecl where
  prettyPrintSimple (FunctionDecl retType funname params) =
    prettyPrintSimple retType ++
    " " ++
    prettyPrintSimple funname ++
    paren (List.intercalate ", " (map prettyPrintSimple params))

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

hangingStatement :: Int -> Statement -> String
hangingStatement n (StatementBlock stmts) =
  " {\n" ++ printProgram (n + 1) stmts ++ "\n" ++ indent n "}"
hangingStatement n s = "\n" ++ prettyPrintStatement (n + 1) s

prettyPrintStatement :: Int -> Statement -> String
prettyPrintStatement n (StatementFunctionCall fcall) =
  indent n (prettyPrintSimple fcall ++ ";")
prettyPrintStatement n (StatementVarDecl varDecl) =
  indent n (prettyPrintSimple varDecl ++ ";")
prettyPrintStatement n (StatementVarDef varDecl expr) =
  indent n (prettyPrintSimple varDecl ++ " = " ++ prettyPrintExpr 0 expr ++ ";")
prettyPrintStatement n (StatementFunctionDecl funDecl) =
  indent n (prettyPrintSimple funDecl ++ ";")
prettyPrintStatement n (StatementForeignFunctionDecl funDecl) =
  indent n ("foreign " ++ prettyPrintSimple funDecl ++ ";")
prettyPrintStatement n (StatementAssign var expr) =
  indent n (prettyPrintSimple var ++ " = " ++ prettyPrintExpr 0 expr ++ ";")
prettyPrintStatement n (StatementAssignPlus var expr) =
  indent n (prettyPrintSimple var ++ " += " ++ prettyPrintExpr 0 expr ++ ";")
prettyPrintStatement n (StatementAssignMinus var expr) =
  indent n (prettyPrintSimple var ++ " -= " ++ prettyPrintExpr 0 expr ++ ";")
prettyPrintStatement n (StatementReturn Nothing) = indent n "return;"
prettyPrintStatement n (StatementReturn (Just e)) =
  indent n ("return " ++ prettyPrintExpr 0 e ++ ";")
prettyPrintStatement n (StatementWhile e s) =
  indent n ("while (" ++ prettyPrintExpr 0 e ++ ")") ++ hangingStatement n s
prettyPrintStatement n (StatementIf e s) =
  indent n ("if (" ++ prettyPrintExpr 0 e ++ ")") ++ hangingStatement n s
prettyPrintStatement n (StatementFor v e1 e2 s) =
  indent
    n
    ("for (" ++
     prettyPrintSimple v ++
     " in " ++ prettyPrintExpr 10 e1 ++ ".." ++ prettyPrintExpr 10 e2 ++ ")") ++
  hangingStatement n s
prettyPrintStatement n (StatementFunctionDef decl stmts) =
  indent n (prettyPrintSimple decl) ++ hangingStatement n (StatementBlock stmts)
prettyPrintStatement n (StatementIfElse e s1@(StatementBlock _) s2) =
  indent n ("if (" ++ prettyPrintExpr 0 e ++ ")") ++
  hangingStatement n s1 ++ " } else" ++ hangingStatement n s2
prettyPrintStatement n (StatementIfElse e s1 s2) =
  indent n ("if (" ++ prettyPrintExpr 0 e ++ ")") ++
  hangingStatement n s1 ++ "\n" ++ indent n "else" ++ hangingStatement n s2
prettyPrintStatement n (StatementBlock stmts) =
  indent n "{\n" ++ printProgram (n + 1) stmts ++ "\n" ++ indent n "}"

printProgram :: Int -> [Statement] -> String
printProgram n stmts =
  List.intercalate "\n" (map (prettyPrintStatement n) stmts)
