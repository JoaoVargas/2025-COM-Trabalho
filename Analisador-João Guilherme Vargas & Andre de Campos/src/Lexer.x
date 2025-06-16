{
module Lexer where

import Token
}

%wrapper "basic"

$alpha = [a-zA-Z]
$digit = [0-9]

@id = $alpha [_$alpha$digit]*
@int = $digit+
@double = $digit+(\.$digit+)?
@literal = \"[^ \"]*\"

tokens :-

<0> $white+ ;  

<0> "int" {\s->INT}
<0> "double" {\s->DOUBLE}
<0> "string" {\s->STRING}
<0> "void" {\s->VOID}

<0> "(" {\s->LPAR}
<0> ")" {\s->RPAR}
<0> "{" {\s->LCURL}
<0> "}" {\s->RCURL}

<0> "," {\s->COMMA}
<0> ";" {\s->SEMICOLON}

<0> "return" {\s->RETURN}
<0> "if" {\s->IF}
<0> "else" {\s->ELSE}
<0> "while" {\s->WHILE}
<0> "=" {\s->ATRIBUTION}
<0> "print" {\s->PRINT}
<0> "read" {\s->READ}

<0> "+" {\s -> ADD}
<0> "-" {\s -> SUB}  
<0> "*" {\s -> MUL}  
<0> "/" {\s -> DIV}  

<0> "&&" {\s -> AND}
<0> "||" {\s -> OR}
<0> "!" {\s -> NOT}
<0> "==" {\s -> EQUAL}
<0> "/=" {\s -> NOT_EQUAL}
<0> ">=" {\s -> GREATER_THAN_EQUAL}
<0> "<=" {\s -> LESS_THAN_EQUAL}
<0> ">" {\s -> GREATER_THAN}
<0> "<" {\s -> LESS_THAN}

<0> @id {\s -> CONST_ID s}
<0> @int {\s -> CONST_INT (read s)}
<0> @double {\s -> CONST_DOUBLE (read s)}
<0> @literal {\s -> CONST_LITERAL s}
{
-- As acoes tem tipo :: String -> Token

testLex = do s <- getLine
             print (alexScanTokens s)
}