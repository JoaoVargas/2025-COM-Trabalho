module AST where

-- Definição de tipos básicos
type Id = String

-- Tipos de dados suportados
data Tipo = TDouble | TInt | TString | TVoid
  deriving (Show, Eq)

-- Constantes literais
data TCons = CDouble Double | CInt Int
  deriving (Show)

-- Expressões aritméticas
data Expr = Add Expr Expr 
          | Sub Expr Expr 
          | Mul Expr Expr 
          | Div Expr Expr 
          | Neg Expr
          | Const TCons 
          | IdVar String 
          | Chamada Id [Expr] 
          | Lit String
          | IntDouble Expr  -- Conversão de int para double
          | DoubleInt Expr  -- Conversão de double para int
  deriving (Show)

-- Expressões relacionais
data ExprR = Req Expr Expr  -- ==
           | Rdif Expr Expr -- !=
           | Rlt Expr Expr  -- <
           | Rgt Expr Expr  -- >
           | Rle Expr Expr  -- <=
           | Rge Expr Expr  -- >=
  deriving (Show)

-- Expressões lógicas
data ExprL = And ExprL ExprL  -- &&
           | Or ExprL ExprL   -- ||
           | Not ExprL        -- !
           | Rel ExprR        -- Expressão relacional
  deriving (Show)

-- Declaração de variáveis (nome :#: (tipo, tamanho para arrays))
data Var = Id :#: (Tipo, Int)
  deriving (Show)

-- Assinatura de função (nome :->: (parâmetros, tipo de retorno))
data Funcao = Id :->: ([Var], Tipo)
  deriving (Show)

-- Estrutura do programa completo
data Programa = Prog [Funcao]        -- Declarações de funções
                         [(Id, [Var], Bloco)]  -- Definições de funções
                         [Var]       -- Variáveis globais
                         Bloco      -- Bloco principal
  deriving (Show)

-- Bloco é uma lista de comandos
type Bloco = [Comando]

-- Comandos da linguagem
data Comando = If ExprL Bloco Bloco     -- if (cond) then {bloco} else {bloco}
             | While ExprL Bloco       -- while (cond) {bloco}
             | Atrib Id Expr            -- id = expr
             | Leitura Id               -- leia id
             | Imp Expr                 -- imprima expr
             | Ret (Maybe Expr)        -- retorne expr ou retorne (se void)
             | Proc Id [Expr]           -- chamada de procedimento
  deriving (Show)