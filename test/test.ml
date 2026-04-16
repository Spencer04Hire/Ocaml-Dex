(* 
  Run the test suite using 
    dune build test
    dune exec test/test.exe

  We encourage you to add more of your own tests to ensure your code is working
*)

open OUnit2
open Ast
open Interp
open Input
(* open TypeCheck *)

(* use parsefile to read in files in the external syntax *)
let parsefile f = fst (parse f)

let run_test file result =
  (* let _ = print_endline ("running test on " ^ file) in
  let _ = print_endline ("program: " ^ eToString (parsefile file)) in
  let _ = print_endline ("expected result: " ^ eToString result) in
  let _ = print_endline ("actual result: " ^ eToString (eInterp (parsefile file))) in *)
  assert_equal (eInterp (parsefile file)).eExp result

let translate_tests =
  "test suite" >::: [
    "let_simple" >:: (fun _ ->
        run_test "test/let_simple.odx" (EInt 5));
    "let_arithmetic" >:: (fun _ ->
        run_test "test/let_arithmetic.odx" (EInt 9));
    "let_negative" >:: (fun _ ->
        run_test "test/let_negative.odx" (EInt (-2)));
  ] 

let _ = print_endline "running tests"
  ; run_test_tt_main translate_tests
