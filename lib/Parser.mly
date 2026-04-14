%{
  open Ast
  
%}

// this is a parser for the external "python-like" syntax

%token <Span.t * Var.t> ID
%token <Span.t * Int.t> NUM
%token <Span.t> FUN
%token <Span.t> END
%token <Span.t> APPLY
%token <Span.t> IF
%token <Span.t> FI
%token <Span.t> THEN
%token <Span.t> ELSE
%token <Span.t> CHECK
%token <Span.t> INT
%token <Span.t> INC
%token <Span.t> IS
%token <Span.t> LPAREN
%token <Span.t> RPAREN
%token <Span.t> COMM
%token <Span.t> COL
%token <Span.t> ARROW 
%token EOF

%start prog
%type  <Ast.exp> prog

%%

expr:
    | ID            { aexp (EVar (snd $1)) (fst $1) }
    | NUM           { aexp (EInt (snd $1)) (fst $1) }
    | INC LPAREN expr RPAREN      
            { aexp (EOp (Inc, $3)) (Span.extend $1 $4)}
    | FUN ID COL ty IS expr END
            { aexp (ETFun (snd $2, $4, $6)) (Span.extend $1 $7)}
    | APPLY LPAREN expr COMM expr RPAREN
            { aexp (EApp ($3, $5)) (Span.extend $1 $6)}
    | IF expr THEN expr ELSE expr FI
            { aexp (EIf ($2, $4, $6)) (Span.extend $1 $7)}
    | CHECK LPAREN expr COMM ty RPAREN
            { aexp (ECheck ($3, $5)) (Span.extend $1 $6)}

ty:
    | aty            { $1 }
    | aty ARROW ty   {EFunType ($1, $3)}
    

aty:
    | INT           {EIntType}
    | LPAREN ty RPAREN { $2 }

prog:
    | expr EOF           { $1 }
;