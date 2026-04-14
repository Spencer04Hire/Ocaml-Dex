
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
let num = ['0'-'9']*
let wspace = [' ' '\t']

rule token = parse
  | id as s           { ID (position lexbuf, Var.fresh s) }
  | num as n          { NUM (position lexbuf, int_of_string n) }
  | "fun"             { FUN (position lexbuf) }
  | "end"             { END (position lexbuf)}
  | "apply"           { APPLY (position lexbuf)}
  | "if"              { IF (position lexbuf)}
  | "fi"              { FI (position lexbuf)}
  | "then"            { THEN (position lexbuf)}
  | "else"            { ELSE (position lexbuf)}
  | "check"           { CHECK (position lexbuf)}
  | "int"             { INT (position lexbuf)}
  | "inc"             { INC (position lexbuf)}
  | "is"              { IS  (position lexbuf)}
  | "("               { LPAREN (position lexbuf) } 
  | ")"               { RPAREN (position lexbuf) }
  | ","               { COMM (position lexbuf)}
  | ":"               { COL (position lexbuf)}
  | "->"              { ARROW (position lexbuf)}
  | wspace            { token lexbuf }
  | '\n'              { incr_linenum lexbuf; token lexbuf}
  | _ as c            { printf "[Parse Error] Unrecognized character: %c\n" c; token lexbuf }
  | eof		            { EOF }

