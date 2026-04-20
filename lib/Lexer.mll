
{
  open Batteries
  open Parser
  open Printf
  open Span
  exception Eof

  let position lexbuf =
    {fname=(lexbuf.Lexing.lex_start_p).pos_fname; start=Lexing.lexeme_start lexbuf; finish=Lexing.lexeme_end lexbuf}

  let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <-
      { pos with Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
                 Lexing.pos_bol = pos.Lexing.pos_cnum; } ;;

}

let id = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '_' '0'-'9']*
let num = ['0'-'9']+
let wspace = [' ' '\t']

rule token = parse
  | "let"             { LET (position lexbuf) }
  | "in"              { IN (position lexbuf) }
  | "fun"             { FUN (position lexbuf) }
  | "if"              { IF (position lexbuf)}
  | "then"            { THEN (position lexbuf)}
  | "else"            { ELSE (position lexbuf)}
  | "int"             { INT (position lexbuf)}
  | "Fin"             { FIN (position lexbuf)}
  | "for"             { FOR (position lexbuf)}
  | "ord"             { ORD (position lexbuf)}
  | "."               { DOT (position lexbuf)}
  | "@"               { AT (position lexbuf)}
  | "("               { LPAREN (position lexbuf) } 
  | ")"               { RPAREN (position lexbuf) }
  | "["               { LBRACK (position lexbuf) }
  | "]"               { RBRACK (position lexbuf) }
  | ":"               { COL (position lexbuf)}
  | "->"              { ARROW (position lexbuf)}
  | "=>"              { ARR_ARROW (position lexbuf)}
  | "="               { EQUAL (position lexbuf) }
  | "+"               { PLUS (position lexbuf)}
  | "-"               { MINUS (position lexbuf)}
  | "*"               { TIMES (position lexbuf) }
  | "/"               { DIV (position lexbuf) }
  | wspace            { token lexbuf }
  | id as s           { ID (position lexbuf, Var.create s) }
  | num as n          { NUM (position lexbuf, int_of_string n) }
  | '\n'              { incr_linenum lexbuf; token lexbuf}
  | _ as c            { printf "[Parse Error] Unrecognized character: %c\n" c; token lexbuf }
  | eof		            { EOF }

