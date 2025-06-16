{
module Parser where

import AST
import Lexer
}

%name parseProgram
%tokentype { Token }
%error { parseError }

%token
    TIf      { TIf }
    TElse    { TElse }
    TWhile   { TWhile }
    TInt     { TInt }
    TFloat   { TFloat }
    TString  { TString }
    TVoid    { TVoid }
    TReturn  { TReturn }
    TPrint   { TPrint }
    TRead    { TRead }
    TAdd     { TAdd }
    TSub     { TSub }
    TMul     { TMul }
    TDiv     { TDiv }
    TAnd     { TAnd }
    TOr      { TOr }
    TNot     { TNot }
    TEq      { TEq }
    TNeq     { TNeq }
    TLt      { TLt }
    TGt      { TGt }
    TLe      { TLe }
    TGe      { TGe }
    TAssign  { TAssign }
    TSemi    { TSemi }
    TComma   { TComma }
    TLParen  { TLParen }
    TRParen  { TRParen }
    TLBrace  { TLBrace }
    TRBrace  { TRBrace }
    TId      { TId $$ }
    TIntConst { TIntConst $$ }
    TFloatConst { TFloatConst $$ }
    TStringConst { TStringConst $$ }

%%

Programa : ListaFuncoes BlocoPrincipal   { Prog $1 [] $2 }
         | BlocoPrincipal               { Prog [] [] $1 }

ListaFuncoes : ListaFuncoes Funcao      { $1 ++ [$2] }
             | Funcao                   { [$1] }

Funcao : TipoRetorno TId TLParen TRParen BlocoPrincipal { ($2, ($1, []), $5 }
       | TipoRetorno TId TLParen DeclParameters TRParen BlocoPrincipal { ($2, ($1, $4), $6 }

TipoRetorno : Tipo                      { $1 }
            | TVoid                     { TVoid }

DeclParameters : DeclParameters TComma Parametro { $1 ++ [$3] }
               | Parametro                       { [$1] }

Parametro : Tipo TId                    { ($2, $1) }

BlocoPrincipal : TLBrace Declaracoes ListaCmd TRBrace { ($2, $3) }
               | TLBrace ListaCmd TRBrace             { ([], $2) }

Declaracoes : Declaracoes Declaracao    { $1 ++ $2 }
            | Declaracao                { $1 }

Declaracao : Tipo ListaId TSemi         { map (\id -> (id, $1)) $2 }

ListaId : ListaId TComma TId            { $1 ++ [$3] }
        | TId                           { [$1] }

Bloco : TLBrace ListaCmd TRBrace        { $2 }

ListaCmd : ListaCmd Comando             { $1 ++ [$2] }
         | Comando                      { [$1] }

Comando : CmdSe                         { $1 }
        | CmdEnquanto                   { $1 }
        | CmdAtrib                     { $1 }
        | CmdEscrita                    { $1 }
        | CmdLeitura                   { $1 }
        | ChamadaProc                   { $1 }
        | Retorno                       { $1 }

{
parseError :: [Token] -> a
parseError _ = error "Erro de sintaxe"
}