%{
  open Ast
%}

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
%token <Span.t> FIN
%token <Span.t> FOR
%token <Span.t> ORD
%token <Span.t> GET
%token <Span.t> RUNSTATE
%token <Span.t> DOT
%token <Span.t> AT
%token <Span.t> LPAREN
%token <Span.t> RPAREN
%token <Span.t> LBRACK
%token <Span.t> RBRACK
%token <Span.t> ASSIGN
%token <Span.t> COL
%token <Span.t> ARROW
%token <Span.t> ARR_ARROW
%token <Span.t> PLUS
%token <Span.t> MINUS
%token <Span.t> TIMES
%token <Span.t> DIV
%token EOF

%start prog
%type  <Ast.exp> prog

%%

expr:
    | addExpr       { $1 }
    | addExpr ARROW expr     { aexp (EFunTypeExpr ($1, $3)) (Span.extend $1.espan $3.espan) }
    | addExpr ARR_ARROW expr { aexp (EArrTypeExpr ($1, $3)) (Span.extend $1.espan $3.espan) }
    | FUN ID COL expr EQUAL expr
            { aexp (ETFun (snd $2, $4, $6)) (Span.extend $1 $6.espan)}
    | IF expr THEN expr ELSE expr
            { aexp (EIf ($2, $4, $6)) (Span.extend $1 $6.espan)}
    | LET ID EQUAL expr IN expr
            { aexp (ELet (snd $2, $4, $6)) (Span.extend $1 $6.espan) }
    | FOR ID COL expr DOT expr
            { aexp (EFor (snd $2, $4, $6)) (Span.extend $1 $6.espan) }
    | RUNSTATE ID ASSIGN expr DOT expr
            { aexp (ERunState ($4, snd $2, $6)) (Span.extend $1 $6.espan) }
    | atomExpr ASSIGN expr
            { aexp (EPut ($1, $3)) (Span.extend $1.espan $3.espan) }

addExpr:
    | mulExpr               { $1 }
    | addExpr PLUS mulExpr  { aexp (EPlus ($1, $3)) (Span.extend $1.espan $3.espan) }
    | addExpr MINUS mulExpr { aexp (EMinus ($1, $3)) (Span.extend $1.espan $3.espan) }

mulExpr:
    | negExpr               { $1 }
    | mulExpr TIMES negExpr { aexp (ETimes ($1, $3)) (Span.extend $1.espan $3.espan) }
    | mulExpr DIV negExpr   { aexp (EDiv ($1, $3)) (Span.extend $1.espan $3.espan) }

negExpr:
    | appExpr               { $1 }
    | MINUS negExpr         { aexp (EMinus (aexp (EInt 0) $1, $2)) (Span.extend $1 $2.espan) }

appExpr:
    | atomExpr              { $1 }
    | appExpr atomExpr      { aexp (EApp ($1, $2)) (Span.extend $1.espan $2.espan) }
    | appExpr LBRACK expr RBRACK { aexp (EArrIndex ($1, $3)) (Span.extend $1.espan $4) }

atomExpr:
    | ID                 { aexp (EVar (snd $1)) (fst $1) }
    | NUM                { aexp (EInt (snd $1)) (fst $1) }
    | INT                { aexp EIntTypeExpr $1 }
    | FIN NUM            { aexp (EFinTypeExpr (aexp (EInt (snd $2)) (fst $2))) (Span.extend $1 (fst $2)) }
    | ORD atomExpr       { aexp (EOrd $2) (Span.extend $1 $2.espan) }
    | NUM AT atomExpr    { aexp (EFromOrd (aexp (EInt (snd $1)) (fst $1), $3)) (Span.extend (fst $1) $3.espan) }
    | GET atomExpr       { aexp (EGet $2) (Span.extend $1 $2.espan) }
    | LPAREN expr RPAREN { $2 }

prog:
    | expr EOF           { $1 }
;