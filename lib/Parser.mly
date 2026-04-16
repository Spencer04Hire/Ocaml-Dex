%{
  open Ast
  
%}

// this is a parser for the external "python-like" syntax

%token <Span.t * Var.t> ID
%token <Span.t * Int.t> NUM
%token <Span.t> LET
%token <Span.t> IN
%token <Span.t> EQUAL
%token <Span.t> FUN
%token <Span.t> IF
%token <Span.t> THEN
%token <Span.t> ELSE
%token <Span.t> INT
%token <Span.t> LPAREN
%token <Span.t> RPAREN
%token <Span.t> COL
%token <Span.t> ARROW
%token <Span.t> PLUS
%token <Span.t> MINUS
%token EOF

%start prog
%type  <Ast.exp> prog

%%

expr:
    | addExpr       { $1 }
    | FUN ID COL ty EQUAL expr
            { aexp (ETFun (snd $2, $4, $6)) (Span.extend $1 $6.espan)}
    | IF expr THEN expr ELSE expr
            { aexp (EIf ($2, $4, $6)) (Span.extend $1 $6.espan)}
    | LET ID EQUAL expr IN expr
            { aexp (ELet (snd $2, $4, $6)) (Span.extend $1 $6.espan) }


addExpr:
    | negExpr               { $1 }
    | addExpr PLUS negExpr  { aexp (EPlus ($1, $3)) (Span.extend $1.espan $3.espan) }
    | addExpr MINUS negExpr { aexp (EMinus ($1, $3)) (Span.extend $1.espan $3.espan) }

negExpr:
    | appExpr               { $1 }
    | MINUS negExpr         { aexp (EMinus (aexp (EInt 0) $1, $2)) (Span.extend $1 $2.espan) }

appExpr:
    | atomExpr              { $1 }
    | appExpr atomExpr      { aexp (EApp ($1, $2)) (Span.extend $1.espan $2.espan) }

atomExpr:
    | ID                 { aexp (EVar (snd $1)) (fst $1) }
    | NUM                { aexp (EInt (snd $1)) (fst $1) }
    | LPAREN expr RPAREN { $2 }

ty:
    | aty            { $1 }
    | aty ARROW ty   {EFunType ($1, $3)}
    

aty:
    | INT           {EIntType}
    | LPAREN ty RPAREN { $2 }

prog:
    | expr EOF           { $1 }
;