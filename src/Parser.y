{
module Parser
  ( parseExpr
  ) where

import Lexer
import Syntax
}

%name program
%tokentype { Token }
%error { parseError }

%token
  ';'
    { TokenSemi _ }
  ','
    { TokenComma _ }
  '('
    { TokenLParen _ }
  ')'
    { TokenRParen _ }
  '{'
    { TokenLBrace _ }
  '}'
    { TokenRBrace _ }
  intlit
    { TokenIntLiteral _ $$ }
  floatlit
    { TokenFloatLiteral _ $$ }
  strlit
    { TokenStringLiteral _ $$ }
  '+'
    { TokenPlus _ }
  '-'
    { TokenMinus _ }
  '*'
    { TokenTimes _ }
  '/'
    { TokenDiv _ }
  '%'
    { TokenMod _ }
  '=='
    { TokenEq _ }
  '!='
    { TokenNeq _ }
  '<'
    { TokenLt _ }
  '<='
    { TokenLeq _ }
  '>'
    { TokenGt _ }
  '>='
    { TokenGeq _ }
  '='
    { TokenAssign _ }
  '..'
    { TokenRange _ }
  'in'
    { TokenIn _ }
  'if'
    { TokenIf _ }
  'else'
    { TokenElse _ }
  'for'
    { TokenFor _ }
  'while'
    { TokenWhile _ }
  'int'
    { TokenInt _ }
  'float'
    { TokenFloat _ }
  'string'
    { TokenString _ }
  'void'
    { TokenVoid _ }
  'return'
    { TokenReturn _ }
  sym
    { TokenSym _ $$ }

%left '==' '!=' '<' '<=' '>' '>='
%left '+' '-'
%left '*' '/' '%'
%left NEG

%%

stmts
  : {- empty -}
    { [] }
  | stmts stmt
    { $2 : $1 }
  | stmts defdecl
    { $2 : $1 }

stmt
  : '{' stmts '}'
    { StatementBlock (reverse $2) }
  | funcall ';'
    { StatementFunctionCall $1 }
  | 'while' '(' expr ')' stmt
    { StatementWhile $3 $5 }
  | sym '=' expr ';'
    { StatementAssign (VarName $1) $3 }
  | 'if' '(' expr ')' stmt 'else' stmt
    { StatementIfElse $3 $5 $7 }
  | 'if' '(' expr ')' stmt
    { StatementIf $3 $5 }
  | 'for' '(' sym 'in' expr '..' expr ')' stmt
    { StatementFor (VarName $3) $5 $7 $9 }
  | 'return' expr ';'
    { StatementReturn (Just $2) }
  | 'return' ';'
    { StatementReturn Nothing }

defdecl
  : fundecl '{' stmts '}'
    { StatementFunctionDef $1 (reverse $3) }
  | fundecl ';'
    { StatementFunctionDecl $1 }
  | vardecl ';'
    { StatementVarDecl $1 }

fundecl
  : 'void' sym '(' params ')' { FunctionDecl Nothing (FunctionName $2) (reverse $4) }
  | vartype sym '(' params ')' { FunctionDecl (Just $1) (FunctionName $2) (reverse $4) }

params
  : {- empty -}
    { [] }
  | vardecl
    { [$1] }
  | params ',' vardecl
    { $3 : $1 }

funcall : sym '(' args ')' { FunctionCall (FunctionName $1) (reverse $3) }

vardecl : vartype sym { VarDecl $1 (VarName $2) }

vartype
  : 'int'
    { VarTypeInt }
  | 'float'
    { VarTypeFloat }
  | 'string'
    { VarTypeString }

args
  : {- empty -}
    { [] }
  | expr
    { [$1] }
  | args ',' expr
    { $3 : $1 }

expr
  : funcall
    { ExprFunctionCall $1 }
  | sym
    { ExprVar (VarName $1) }
  | intlit
    { ExprInt $1 }
  | floatlit
    { ExprFloat $1 }
  | strlit
    { ExprString $1 }
  | '(' expr ')'
    { $2 }
  | '-' expr %prec NEG
    { ExprNeg $2 }
  | expr '+' expr
    { ExprPlus $1 $3 }
  | expr '-' expr
    { ExprMinus $1 $3 }
  | expr '*' expr
    { ExprTimes $1 $3 }
  | expr '/' expr
    { ExprDiv $1 $3 }
  | expr '%' expr
    { ExprMod $1 $3 }
  | expr '==' expr
    { ExprEq $1 $3 }
  | expr '!=' expr
    { ExprNeq $1 $3 }
  | expr '<' expr
    { ExprLt $1 $3 }
  | expr '<=' expr
    { ExprLeq $1 $3 }
  | expr '>' expr
    { ExprGt $1 $3 }
  | expr '>=' expr
    { ExprGeq $1 $3 }

{
parseError :: [Token] -> a
parseError (l:ls) = error (show l)
parseError [] = error "Unexpected end of Input"

parseExpr :: String -> Program
parseExpr = Program . reverse . program . scanTokens
}
