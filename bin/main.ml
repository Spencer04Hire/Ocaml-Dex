open Ast
open Input
open TypeCheck
open Interp

let () =
  if Array.length Sys.argv < 2 then (
    Printf.printf "Usage: main.exe <filename>\n";
    exit 1
  );
  let fname = Sys.argv.(1) in
  try
    let e = fst (parse fname) in
    let t = typeCheck e in
    Printf.printf "Type: %s\n" (typeString t);
    let result = interp e in
    Printf.printf "Result: %s\n" (eToString result)
  with
  | TypeCheck.TypeError msg -> Printf.printf "[Type Error] %s\n" msg
  | TypeCheck.UnboundVariable msg -> Printf.printf "[Type Error] Unbound variable: %s\n" msg
  | TypeCheck.Impossible -> Printf.printf "[Type Error] Impossible state reached\n"
  | Interp.RuntimeError msg -> Printf.printf "[Runtime Error] %s\n" msg
  | Interp.Impossible -> Printf.printf "[Runtime Error] Impossible state reached\n"
  | Sys_error msg -> Printf.printf "[Error] %s\n" msg
  | Lexer.Eof -> Printf.printf "[Parse Error] Unexpected end of file\n"
