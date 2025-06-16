module Token where

data Token
  = INT
  | DOUBLE
  | STRING
  | VOID
  | LPAR
  | LCURL
  | RCURL
  | RPAR
  | COMMA
  | SEMICOLON
  | RETURN
  | IF
  | ELSE
  | WHILE
  | ATRIBUTION
  | PRINT
  | READ
  | ADD
  | SUB
  | MUL
  | DIV
  | AND
  | OR
  | NOT
  | EQUAL
  | NOT_EQUAL
  | GREATER_THAN_EQUAL
  | LESS_THAN_EQUAL
  | GREATER_THAN
  | LESS_THAN
  | CONST_ID String
  | CONST_INT Int
  | CONST_DOUBLE Double
  | CONST_LITERAL String
  deriving (Eq, Show)
