{
module Parser where

import Token
import qualified Lexer as L
import DataTypes
}


%name eval
%tokentype { Token }
%error { parseError }
%token 
'int' {INT}
'double' {DOUBLE}
'string' {STRING}
'void' {VOID}
'(' {LPAR}
')' {RPAR}
'{' {LCURL}
'}' {RCURL}
',' {COMMA}
';' {SEMICOLON}
'return' {RETURN}
'if' {IF}
'else' {ELSE}
'while' {WHILE}
'=' {ATRIBUTION}
'print' {PRINT}
'read' {READ}
'+' {ADD}
'-' {SUB}  
'*' {MUL}  
'/' {DIV}  
'&&' {AND}
'||' {OR}
'!' {NOT}
'==' {EQUAL}
'/=' {NOT_EQUAL}
'>=' {GREATER_THAN_EQUAL}
'<=' {LESS_THAN_EQUAL}
'>' {GREATER_THAN}
'<' {LESS_THAN}

Id {CONST_ID $$}
Int {CONST_INT $$}
Double {CONST_DOUBLE $$}
Literal {CONST_LITERAL $$}

%left '||' '&&'
%left '==' '/=' '<=' '>=' '<' '>'
%left '+' '-'
%left '*' '/'

%%

Programa 
  : ListaFuncoes BlocoPrincipal {Prog (map fst $1) (map t1 $1) (fst $2) (snd $2)}
  | BlocoPrincipal {Prog[] [] (fst $1) (snd $1)}

ListaFuncoes
  : ListaFuncoes Funcao {$1 ++ [$2]}
  | Funcao {[$1]}

Funcao
  : TipoRetorno Id '(' DeclParametros ')' BlocoPrincipal {($2:->:($4,$1), $6)}
  | TipoRetorno Id '(' ')' BlocoPrincipal {($2 :->:([],$1), $5)}

TipoRetorno
  : Tipo {$1}
  | 'void' {TVoid}

DeclParametros
  : DeclParametros ',' Parametro {$1 ++ [$3]}
  | Parametro {[$1]}

Parametro: Tipo Id {$2:#:($1, 0)}

BlocoPrincipal
  : '{' Declaracoes ListaCmd '}' {($2, $3)}
  | '{' ListaCmd '}' {([], $2)}

Declaracoes
  : Declaracoes Declaracao {$1 ++ $2}
  | Declaracao {$1}

Declaracao: Tipo ListaId ';'{map (\x -> x:#: ($1,0)) $2}

Tipo
  : 'int' {TInt}
  | 'string' {TString}
  | 'double' {TDouble}

ListaId
  : ListaId ',' Id {$1 ++ [$3]}
  | Id {[$1]}

Bloco: '{' ListaCmd '}' {$2}

ListaCmd
  : ListaCmd Comando {$1++[$2]}
  | Comando {[$1]}

Comando
  : CmdSe {$1} 
  | CmdEnquanto {$1}
  | CmdAtrib {$1}
  | CmdEscrita {$1}
  | CmdLeitura {$1}
  | ChamadaProc {$1}
  | Retorno {$1}

Retorno
  : 'return' ExpressaoAritimetica ';' {Ret (Just $2)}
  | 'return' Literal ';' {Ret (Just (Lit $2))}
  | 'return' ';' {Ret (Nothing)}

CmdSe
  : 'if' '(' ExpressaoLogica ')' Bloco {If $3 $5 []}
  | 'if' '(' ExpressaoLogica ')' Bloco 'else' Bloco {If $3 $5 $7} 

CmdEnquanto: 'while' '(' ExpressaoLogica ')' Bloco {While $3 $5}

CmdAtrib
  : Id '=' ExpressaoAritimetica ';' {Atrib $1 $3}
  | Id '=' Literal ';'{Atrib $1 (Lit $3)}

CmdEscrita
  : 'print' '(' ExpressaoAritimetica ')' ';' {Imp $3} 
  | 'print' '(' Literal ')' ';'        {Imp (Lit $3)}  

CmdLeitura : 'read' '(' Id ')' ';' {Leitura $3}

ChamadaProc: ChamadaFuncao ';' {Proc (fst $1) (snd $1)}

ChamadaFuncao
  : Id '(' ListaParametros ')' {($1,$3)}
  | Id '(' ')'              {($1,[])}

ListaParametros
  : ListaParametros ',' ExpressaoAritimetica {$1 ++ [$3]}
  | ListaParametros ',' Literal {$1 ++ [Lit $3]} 
  | ExpressaoAritimetica {[$1]} 
  | Literal {[Lit $1]}


ExpressaoLogica
  : ExpressaoLogica '&&' ExpressaoLogica {And $1 $3}
  | ExpressaoLogica '||' ExpressaoLogica {Or $1 $3}
  | '!' ExpressaoLogica {Not $2}
  | '(' ExpressaoLogica ')' {$2}
  | ExpressaoRelacional {Rel $1}

ExpressaoRelacional
  : ExpressaoAritimetica '>' ExpressaoAritimetica {Req $1 $3}
  | ExpressaoAritimetica '<' ExpressaoAritimetica {Rge $1 $3}
  | ExpressaoAritimetica '>=' ExpressaoAritimetica {Rgt $1 $3}
  | ExpressaoAritimetica '<=' ExpressaoAritimetica {Rlt $1 $3}
  | ExpressaoAritimetica '==' ExpressaoAritimetica {Rle $1 $3}
  | ExpressaoAritimetica '/=' ExpressaoAritimetica {Rdif $1 $3}

ExpressaoAritimetica
  : '(' ExpressaoAritimetica ')' {$2}
  | ExpressaoAritimetica '+' ExpressaoAritimetica {Add $1 $3}
  | ExpressaoAritimetica '-' ExpressaoAritimetica {Sub $1 $3}
  | ExpressaoAritimetica '*' ExpressaoAritimetica {Mul $1 $3}
  | ExpressaoAritimetica '/' ExpressaoAritimetica {Div $1 $3}
  | Int {Const (CInt $1)}
  | Double {Const (CDouble $1)}
  | Id {IdVar $1}
  | ChamadaFuncao {Chamada (fst $1) (snd $1)}
  | '(' 'int' ')' ExpressaoAritimetica { DoubleInt $4 }


{
parseError :: [Token] -> a
parseError s = error ("Parse error:" ++ show s)


buildAST input = eval (L.alexScanTokens input)

main :: IO ()
main = do
    putStrLn "Digite o caminho do arquivo com o código:"
    path <- getLine
    input <- readFile path
    writeFile "AST.txt" (show $ buildAST input)
}