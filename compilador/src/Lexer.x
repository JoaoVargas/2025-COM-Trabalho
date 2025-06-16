{
module Lexer where

import AST
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$alnum = [a-zA-Z0-9]

tokens :-
  $white+                   ;
  "--".*                    ; -- Comentários de linha
  "if"                      { \s -> TIf }
  "else"                    { \s -> TElse }
  "while"                   { \s -> TWhile }
  "int"                     { \s -> TInt }
  "float"                   { \s -> TFloat }
  "string"                  { \s -> TString }
  "void"                    { \s -> TVoid }
  "return"                  { \s -> TReturn }
  "print"                   { \s -> TPrint }
  "read"                    { \s -> TRead }
  "+"                       { \s -> TAdd }
  "-"                       { \s -> TSub }
  "*"                       { \s -> TMul }
  "/"                       { \s -> TDiv }
  "&&"                      { \s -> TAnd }
  "||"                      { \s -> TOr }
  "!"                       { \s -> TNot }
  "=="                      { \s -> TEq }
  "/="                      { \s -> TNeq }
  "<"                       { \s -> TLt }
  ">"                       { \s -> TGt }
  "<="                      { \s -> TLe }
  ">="                      { \s -> TGe }
  "="                       { \s -> TAssign }
  ";"                       { \s -> TSemi }
  ","                       { \s -> TComma }
  "("                       { \s -> TLParen }
  ")"                       { \s -> TRParen }
  "{"                       { \s -> TLBrace }
  "}"                       { \s -> TRBrace }
  $alpha $alnum*           { \s -> TId s }
  $digit+                  { \s -> TIntConst (read s) }
  $digit+ "." $digit+      { \s -> TFloatConst (read s) }
  \"[^\"]*\"               { \s -> TStringConst (init (tail s)) }

{
-- Função auxiliar para executar o lexer
lexer :: String -> [Token]
lexer = alexScanTokens
}