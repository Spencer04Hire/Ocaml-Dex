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
  let path = "test/tests/" ^ file in
  let e = parsefile path in
  try
    let _ = typeOf Context.empty e in
    assert_failure ("expected unbound variable error in " ^ file)
  with UnboundVariable _ -> ()

let type_check_fail file =
  let path = "test/tests/" ^ file in
  let e = parsefile path in
  try
    let _ = typeOf Context.empty e in
    assert_failure ("expected type error in " ^ file)
  with TypeError _ -> ()

let runtime_check_fail file =
  let path = "test/tests/" ^ file in
  let e = parsefile path in
  try
    let _ = typeOf Context.empty e in
    let _ = eInterp e in
    assert_failure ("expected runtime error in " ^ file)
  with RuntimeError _ -> ()

let equal e1 e2 =
  let s1 = eToString e1 in
  let s2 = eToString e2 in
  s1 = s2

let run_test file result =
  (* let _ = print_endline ("running test on " ^ file) in
  let _ = print_endline ("program: " ^ eToString (parsefile file)) in
  let _ = print_endline ("expected result: " ^ eToString result) in
  let _ = print_endline ("actual result: " ^ eToString (eInterp (parsefile file))) in *)
  let path = "test/tests/" ^ file in
  let exp_result = expOf result in
  let e = parsefile path in
  let _ = typeOf Context.empty e in
  assert_bool "not equal" (equal (eInterp e) exp_result)

let five_ones = EArr [expOf (EInt 1); expOf (EInt 1); expOf (EInt 1); expOf (EInt 1); expOf (EInt 1)]

let nested_five_ones = EArr [expOf five_ones; expOf five_ones; expOf five_ones; expOf five_ones; expOf five_ones]

let tests =
  "test suite" >::: [
    "let_simple" >:: (fun _ ->
        run_test "let_simple.odx" (EInt 5));
    "let_arithmetic" >:: (fun _ ->
        run_test "let_arithmetic.odx" (EInt 9));
    "let_negative" >:: (fun _ ->
        run_test "let_negative.odx" (EInt (-2)));
    "func_arg_type_fail" >:: (fun _ ->
        type_check_fail "func_arg_type_fail.odx");
    "unbound_var" >:: (fun _ ->
        check_var_fail "unbound_var.odx");
    "arithmetic_type_fail" >:: (fun _ ->
        type_check_fail "arithmetic_type_fail.odx");
    "app_type_fail" >:: (fun _ ->
        type_check_fail "app_type_fail.odx");
    "if_cond_type_fail" >:: (fun _ ->
        type_check_fail "if_cond_type_fail.odx");
    "if_branch_type_fail" >:: (fun _ ->
        type_check_fail "if_branch_type_fail.odx");
    "let_fin" >:: (fun _ ->
        run_test "let_fin.odx" (EFinTypeExpr (expOf (EInt 5))));
    "fin_zero_fail" >:: (fun _ ->
        type_check_fail "fin_zero_fail.odx");
    "for_simple" >:: (fun _ ->
        run_test "for_simple.odx" five_ones);
    "for_type_let" >:: (fun _ ->
        run_test "for_type_let.odx" five_ones);
    "for_nested" >:: (fun _ ->
        run_test "for_nested.odx" nested_five_ones);
    "for_type_fail" >:: (fun _ ->
        type_check_fail "for_type_fail.odx");
    "index_simple" >:: (fun _ ->
        run_test "index_simple.odx" five_ones);
    "index_type_fail" >:: (fun _ ->
        type_check_fail "index_type_fail.odx");
    "index_arr_type_fail" >:: (fun _ ->
        type_check_fail "index_arr_type_fail.odx");
    "index_2d" >:: (fun _ ->
        run_test "index_2d.odx" nested_five_ones);
    "div_simple" >:: (fun _ ->
        run_test "div_simple.odx" (EInt 3));
    "mul_simple" >:: (fun _ ->
        run_test "mul_simple.odx" (EInt 21));
    "arithemtic_simple" >:: (fun _ ->
        run_test "arithmetic_simple.odx" (EInt 2));
    "div_zero_fail" >:: (fun _ ->
        runtime_check_fail "div_zero_fail.odx");
    "variable_fun_type" >:: (fun _ ->
        run_test "variable_fun_type.odx" (EInt 2));
  ] 

let _ = print_endline "running tests"
  ; run_test_tt_main tests
