{-# LANGUAGE FlexibleInstances #-}

module PrettyPrint
  ( prettyPrint
  ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.List as List

import ASMSyntax
import Value (Value)

prettyPrint :: Program -> String
prettyPrint p =
  unlines
    [ printLibs $ programLibraries p
    , printForeignFunctions $ programForeignFunctions p
    , printConstants $ programConstants p
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
    (\key val rest -> rest ++ "\n" ++ show key ++ ": " ++ prettyPrintSimple val)
    ""
    funs

printConstants :: IntMap Value -> String
printConstants vals =
  "Constants: " ++
  IntMap.foldrWithKey
    (\key val rest -> rest ++ "\n" ++ show key ++ ": " ++ show val)
    ""
    vals

paren :: String -> String
paren str = "(" ++ str ++ ")"

class PrettyPrintSimple a where
  prettyPrintSimple :: a -> String

instance PrettyPrintSimple Var where
  prettyPrintSimple v = "RBP + " ++ show (varDisplacement v)

instance PrettyPrintSimple FunID where
  prettyPrintSimple = show

instance PrettyPrintSimple LabelID where
  prettyPrintSimple = show

instance PrettyPrintSimple FunctionCall where
  prettyPrintSimple NativeFunctionCall { nativeFunCallName = funname
                                       , nativeFunCallArgs = args
                                       } =
    prettyPrintSimple funname ++
    paren (List.intercalate ", " (map prettyPrintSimple args))
  prettyPrintSimple ForeignFunctionCall { foreignFunCallName = funname
                                        , foreignFunCallArgs = args
                                        } =
    prettyPrintSimple funname ++
    paren (List.intercalate ", " (map prettyPrintSimple args))

instance PrettyPrintSimple VarType where
  prettyPrintSimple = show

instance PrettyPrintSimple (Maybe VarType) where
  prettyPrintSimple (Just vtype) = prettyPrintSimple vtype
  prettyPrintSimple Nothing = "void"

instance PrettyPrintSimple BinOp where
  prettyPrintSimple BinPlus = "+"
  prettyPrintSimple BinMinus = "-"
  prettyPrintSimple BinTimes = "*"
  prettyPrintSimple BinDiv = "/"
  prettyPrintSimple BinMod = "%"
  prettyPrintSimple BinBitAnd = "&"
  prettyPrintSimple BinBitOr = "|"
  prettyPrintSimple BinBitXor = "^"
  prettyPrintSimple BinAnd = "&&"
  prettyPrintSimple BinOr = "||"
  prettyPrintSimple BinEq = "=="
  prettyPrintSimple BinLt = "<"

instance PrettyPrintSimple UnOp where
  prettyPrintSimple UnNeg = "-"
  prettyPrintSimple UnNot = "!"
  prettyPrintSimple UnIntToFloat = "(double)"

instance PrettyPrintSimple Expr where
  prettyPrintSimple (ExprRead v) = prettyPrintSimple v
  prettyPrintSimple (ExprDereference p) = "*" ++ prettyPrintSimple p
  prettyPrintSimple (ExprConst _ c) = show c
  prettyPrintSimple (ExprUnOp op v) =
    prettyPrintSimple op ++ prettyPrintSimple v
  prettyPrintSimple (ExprBinOp op el er) =
    prettyPrintSimple el ++
    " " ++ prettyPrintSimple op ++ " " ++ prettyPrintSimple er

instance PrettyPrintSimple Operand where
  prettyPrintSimple (OperandRegister _ r) = prettyPrintSimple r
  prettyPrintSimple (OperandPointer p) = prettyPrintSimple p
  prettyPrintSimple (OperandImmediateInt i) = show i

instance PrettyPrintSimple Register where
  prettyPrintSimple RegisterRSP = "RSP"
  prettyPrintSimple RegisterRBP = "RBP"
  prettyPrintSimple RegisterRAX = "RAX"
  prettyPrintSimple RegisterXMM0 = "XMM0"

instance PrettyPrintSimple Pointer where
  prettyPrintSimple Pointer {pointerBase = mr, pointerDisplacement = d} =
    "[" ++ (maybe "" ((++ "+") . prettyPrintSimple) mr) ++ show d ++ "]"

instance PrettyPrintSimple Statement where
  prettyPrintSimple (StatementFunctionCall fcall) =
    prettyPrintSimple fcall ++ ";"
  prettyPrintSimple (StatementAssign var expr) =
    prettyPrintSimple var ++ " = " ++ prettyPrintSimple expr ++ ";"
  prettyPrintSimple (StatementAssignToPtr ptr var) =
    "*" ++ prettyPrintSimple ptr ++ " = " ++ prettyPrintSimple var ++ ";"
  prettyPrintSimple (StatementPushOnStack x) =
    "push " ++ prettyPrintSimple x ++ ";"
  prettyPrintSimple (StatementAllocateOnStack t) =
    "alloc " ++ prettyPrintSimple t ++ ";"
  prettyPrintSimple (StatementPopFromStack t) =
    "pop " ++ prettyPrintSimple t ++ ";"
  prettyPrintSimple StatementReturn = "return;"
  prettyPrintSimple (StatementLabel l) = show l ++ ": nop;"
  prettyPrintSimple (StatementJump l) = "jmp " ++ show l ++ ";"
  prettyPrintSimple (StatementJumpIfZero v l) =
    "jz (" ++ prettyPrintSimple v ++ ") " ++ show l ++ ";"

instance PrettyPrintSimple FunctionDef where
  prettyPrintSimple fdef =
    prettyPrintSimple (funDefRetType fdef) ++
    " " ++
    prettyPrintSimple (funDefName fdef) ++
    paren (List.intercalate ", " (map prettyPrintSimple (funDefParams fdef))) ++
    "\n" ++
    "\nbefore {\n" ++
    List.intercalate
      "\n"
      (map (indent . prettyPrintSimple) (funDefBeforeBody fdef)) ++
    "\n}" ++
    "\n{\n" ++
    List.intercalate "\n" (map (indent . prettyPrintSimple) (funDefBody fdef)) ++
    "\n}" ++
    "\nafter {\n" ++
    List.intercalate
      "\n"
      (map (indent . prettyPrintSimple) (funDefAfterBody fdef)) ++
    "\n}"
    where
      indent :: String -> String
      indent = ("  " ++)
