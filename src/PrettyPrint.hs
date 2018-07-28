{-# LANGUAGE FlexibleInstances #-}

module PrettyPrint
  ( prettyPrint
  ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.List as List

import LinearSyntax
import Value (Value)

prettyPrint :: Program -> String
prettyPrint p =
  unlines
    [ printLibs $ programLibraries p
    , printForeignFunctions $ programForeignFunctions p
    , printConstants $ programConstants p
    , printVariables $ programVariables p
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
  foreignFunDeclRealName fdecl ++
  " " ++
  show (foreignFunDeclParams fdecl) ++
  (if foreignFunDeclHasVarArgs fdecl
     then " + varargs"
     else "")

printFunctions :: IntMap FunctionDef -> String
printFunctions funs =
  "Functions: " ++
  IntMap.foldrWithKey
    (\key val rest ->
       rest ++ "\n" ++ show key ++ ": " ++ prettyPrintFunctionDef val)
    ""
    funs

printConstants :: IntMap Value -> String
printConstants vals =
  "Constants: " ++
  IntMap.foldrWithKey
    (\key val rest -> rest ++ "\n" ++ show key ++ ": " ++ show val)
    ""
    vals

printVariables :: IntMap VarType -> String
printVariables vals =
  "Variables: " ++
  IntMap.foldrWithKey
    (\key val rest -> rest ++ "\n" ++ show key ++ ": " ++ show val)
    ""
    vals

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
prettyPrintExpr _ (ExprVar _ varname) = prettyPrintSimple varname
prettyPrintExpr _ (ExprDereference _ varname) = "*" ++ prettyPrintSimple varname
prettyPrintExpr _ (ExprAddressOf _ varname) = "&" ++ prettyPrintSimple varname
prettyPrintExpr _ (ExprConst _ c) = show c
prettyPrintExpr n (ExprUnOp op e) =
  parenIfNeeded n (unOpPrec op) $
  prettyPrintUnOp op ++ prettyPrintExpr (unOpPrec op) e
prettyPrintExpr n (ExprBinOp op el er) =
  parenIfNeeded n (binOpPrec op) $
  prettyPrintExpr (binOpPrec op) el ++
  " " ++ prettyPrintBinOp op ++ " " ++ prettyPrintExpr (binOpPrec op) er

class PrettyPrintSimple a where
  prettyPrintSimple :: a -> String

instance PrettyPrintSimple VarID where
  prettyPrintSimple = show

instance PrettyPrintSimple FunID where
  prettyPrintSimple = show

instance PrettyPrintSimple LabelID where
  prettyPrintSimple = show

instance PrettyPrintSimple FunctionCall where
  prettyPrintSimple NativeFunctionCall { nativeFunCallName = funname
                                       , nativeFunCallArgs = args
                                       } =
    prettyPrintSimple funname ++
    paren (List.intercalate ", " (map (prettyPrintExpr 0) args))
  prettyPrintSimple ForeignFunctionCall { foreignFunCallName = funname
                                        , foreignFunCallArgs = args
                                        } =
    prettyPrintSimple funname ++
    paren (List.intercalate ", " (map (prettyPrintExpr 0) args))

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
prettyPrintStatement n (StatementAssignToPtr var expr) =
  indent
    n
    ("*" ++ prettyPrintSimple var ++ " = " ++ prettyPrintExpr 0 expr ++ ";")
prettyPrintStatement n (StatementReturn Nothing) = indent n "return;"
prettyPrintStatement n (StatementReturn (Just e)) =
  indent n ("return " ++ prettyPrintExpr 0 e ++ ";")
prettyPrintStatement n (StatementLabel l) = indent n (show l ++ ": nop;")
prettyPrintStatement n (StatementJump l) = indent n ("jmp " ++ show l ++ ";")
prettyPrintStatement n (StatementJumpIfZero e l) =
  indent n ("jz (" ++ prettyPrintExpr 0 e ++ ") " ++ show l ++ ";")

prettyPrintBody :: [Statement] -> String
prettyPrintBody body = "{\n" ++ printProgram 1 body ++ "\n}"

prettyPrintFunctionDef :: FunctionDef -> String
prettyPrintFunctionDef fdef =
  prettyPrintSimple (funDefRetType fdef) ++
  " " ++
  prettyPrintSimple (funDefName fdef) ++
  paren (List.intercalate ", " (map prettyPrintSimple (funDefParams fdef))) ++
  "\n" ++
  List.intercalate
    "\n"
    (map (\v -> "local " ++ prettyPrintSimple v) (funDefLocals fdef)) ++
  "\n" ++ prettyPrintBody (funDefBody fdef)

printProgram :: Int -> [Statement] -> String
printProgram n stmts =
  List.intercalate "\n" (map (prettyPrintStatement n) stmts)
