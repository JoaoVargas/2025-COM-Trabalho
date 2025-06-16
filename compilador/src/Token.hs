module Token where

data Expr = Add Expr Expr  | Sub Expr Expr | Mul Expr Expr | Div Expr Expr | Num Double deriving Show

data Token
  = NUM Double
  | ADD
  | SUB
  | MUL
  | DIV
  | LPAR
  | RPAR
  deriving (Eq, Show)
  
