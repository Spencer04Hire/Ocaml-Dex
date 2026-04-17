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
open TypeCheck

(* use parsefile to read in files in the external syntax *)
let parsefile f = fst (parse f)
  
let check_var_fail file =
  let e = parsefile file in
  try
    let _ = typeOf Context.empty e in
    assert_failure ("expected unbound variable error in " ^ file)
  with UnboundVariable _ -> ()

let type_check_fail file =
  let e = parsefile file in
  try
    let _ = typeOf Context.empty e in
    assert_failure ("expected type error in " ^ file)
  with TypeError _ -> ()

let equal e1 e2 =
  let s1 = eToString e1 in
  let s2 = eToString e2 in
  s1 = s2

let run_test file result =
  (* let _ = print_endline ("running test on " ^ file) in
  let _ = print_endline ("program: " ^ eToString (parsefile file)) in
  let _ = print_endline ("expected result: " ^ eToString result) in
  let _ = print_endline ("actual result: " ^ eToString (eInterp (parsefile file))) in *)
  let exp_result = expOf result in
  let e = parsefile file in
  let _ = typeOf Context.empty e in
  assert_bool "not equal" (equal (eInterp e) exp_result)

let tests =
  "test suite" >::: [
    "let_simple" >:: (fun _ ->
        run_test "test/let_simple.odx" (EInt 5));
    "let_arithmetic" >:: (fun _ ->
        run_test "test/let_arithmetic.odx" (EInt 9));
    "let_negative" >:: (fun _ ->
        run_test "test/let_negative.odx" (EInt (-2)));
    "func_arg_type_fail" >:: (fun _ ->
        type_check_fail "test/func_arg_type_fail.odx");
    "unbound_var" >:: (fun _ ->
        check_var_fail "test/unbound_var.odx");
    "arithmetic_type_fail" >:: (fun _ ->
        type_check_fail "test/arithmetic_type_fail.odx");
    "app_type_fail" >:: (fun _ ->
        type_check_fail "test/app_type_fail.odx");
    "if_cond_type_fail" >:: (fun _ ->
        type_check_fail "test/if_cond_type_fail.odx");
    "if_branch_type_fail" >:: (fun _ ->
        type_check_fail "test/if_branch_type_fail.odx");
    "let_fin" >:: (fun _ ->
        run_test "test/let_fin.odx" (EFin (expOf (EInt 5))));
    "fin_type_fail" >:: (fun _ ->
        type_check_fail "test/fin_type_fail.odx");
    "fin_neg_fail" >:: (fun _ ->
        type_check_fail "test/fin_neg_fail.odx");
  ] 

let _ = print_endline "running tests"
  ; run_test_tt_main tests
