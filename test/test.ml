(* 
  Run the test suite using 
    dune build test
    dune exec test/test.exe

  We encourage you to add more of your own tests to ensure your code is working
*)

open OUnit2
open Ast
(* open Interp *)
(* open Input *)
(* open TypeCheck *)

let op_int_test =
  expOf (EOp (Inc, expOf (EInt 1)))

let etfun_test = 
  let v = Var.fresh "v" in
  expOf (EApp(expOf (ETFun(v, EIntType, expOf (EVar v))),
  expOf (EInt 1)))

let if_int_equal_test =
  expOf (EIf (expOf (EInt 1), expOf (EInt 2), expOf (EInt 3)))


(* use parsefile to read in files in the external syntax *)
(* let parsefile f = fst (parse f) *)

let run_test test = 
  assert_equal test test

let translate_tests =
  "test suite for translate" >::: [
    "increment_one_int" >:: (fun _ ->
        run_test op_int_test);
    "typed_identity" >:: (fun _ ->
        run_test etfun_test);
    "if_int_equal" >:: (fun _ ->
        run_test if_int_equal_test);
  ] 

let _ = print_endline "running tests"
  ; run_test_tt_main translate_tests
