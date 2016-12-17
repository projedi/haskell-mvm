{
{-# OPTIONS_GHC -fno-warn-unused-matches -fno-warn-name-shadowing #-}
module Lexer
  ( Token(..)
  , scanTokens
  ) where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$eol = [\n]
$string = $printable # \"

tokens :-
  $white+
    ;
  "//".*
    ;
  ";"
    { \p s -> TokenSemi p }
  ","
    { \p s -> TokenComma p }
  \(
    { \p s -> TokenLParen p }
  \)
    { \p s -> TokenRParen p }
  \{
    { \p s -> TokenLBrace p }
  \}
    { \p s -> TokenRBrace p }
  "-"?$digit+
    { \p s -> TokenIntLiteral p (read s) }
  "-"?$digit+.$digit+
    { \p s -> TokenFloatLiteral p (read s) }
  \" $string* \"
    { \p s -> TokenStringLiteral p (read s) }
  "+"
    { \p s -> TokenPlus p }
  "-"
    { \p s -> TokenMinus p }
  "*"
    { \p s -> TokenTimes p }
  "/"
    { \p s -> TokenDiv p }
  "%"
    { \p s -> TokenMod p }
  "!"
    { \p s -> TokenNot p }
  "=="
    { \p s -> TokenEq p }
  "!="
    { \p s -> TokenNeq p }
  "<"
    { \p s -> TokenLt p }
  "<="
    { \p s -> TokenLeq p }
  ">"
    { \p s -> TokenGt p }
  ">="
    { \p s -> TokenGeq p }
  "="
    { \p s -> TokenAssign p }
  ".."
    { \p s -> TokenRange p }
  in
    { \p s -> TokenIn p }
  if
    { \p s -> TokenIf p }
  else
    { \p s -> TokenElse p }
  for
    { \p s -> TokenFor p }
  while
    { \p s -> TokenWhile p }
  int
    { \p s -> TokenInt p }
  float
    { \p s -> TokenFloat p }
  string
    { \p s -> TokenString p }
  void
    { \p s -> TokenVoid p }
  return
    { \p s -> TokenReturn p }
  foreign
    { \p s -> TokenForeign p }
  $alpha [$alpha $digit \_ \']*
    { \p s -> TokenSym p s }

{
data Token
  = TokenSemi AlexPosn
  | TokenComma AlexPosn
  | TokenLParen AlexPosn
  | TokenRParen AlexPosn
  | TokenLBrace AlexPosn
  | TokenRBrace AlexPosn
  | TokenIntLiteral AlexPosn Int
  | TokenFloatLiteral AlexPosn Double
  | TokenStringLiteral AlexPosn String
  | TokenPlus AlexPosn
  | TokenMinus AlexPosn
  | TokenTimes AlexPosn
  | TokenDiv AlexPosn
  | TokenMod AlexPosn
  | TokenNot AlexPosn
  | TokenEq AlexPosn
  | TokenNeq AlexPosn
  | TokenLt AlexPosn
  | TokenLeq AlexPosn
  | TokenGt AlexPosn
  | TokenGeq AlexPosn
  | TokenAssign AlexPosn
  | TokenRange AlexPosn
  | TokenIn AlexPosn
  | TokenIf AlexPosn
  | TokenElse AlexPosn
  | TokenFor AlexPosn
  | TokenWhile AlexPosn
  | TokenInt AlexPosn
  | TokenFloat AlexPosn
  | TokenString AlexPosn
  | TokenVoid AlexPosn
  | TokenReturn AlexPosn
  | TokenForeign AlexPosn
  | TokenSym AlexPosn String
  deriving (Eq, Show)

scanTokens = alexScanTokens
}
