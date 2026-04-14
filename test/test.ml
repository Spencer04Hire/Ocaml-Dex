(* 
  Run the test suite using 
    dune build test
    dune exec test/test.exe

  We encourage you to add more of your own tests to ensure your code is working
*)

open OUnit2
open Ast
open Interp
open Translate
(* open Input *)
(* open TypeCheck *)

(* we will never test for equality of functions as values *)
let eiEqVal (e: exp) (i: iExp) =
  match (e.eExp, i) with
  | EInt i, IInt j -> i = j
  | EInt i, II2D (IInt j) -> i = j
  | _ ->  false

let test1 = 
  let v = Var.fresh "v" in
  expOf (EApp(expOf (EUFun(v, expOf (EVar v))), 
  expOf (EInt 1)))

let op_test = 
  let v = Var.fresh "v" in
  expOf (EApp(expOf (EUFun(v, expOf (EOp (Inc, expOf (EVar v))))), 
  expOf (EInt 1)))

let op_int_test =
  expOf (EOp (Inc, expOf (EInt 1)))

let typed_var_fun_test = 
  let v = Var.fresh "v" in
  expOf (EApp(expOf (ETFun(v, EFunTy (EIntTy, EIntTy), expOf (EApp (expOf (EVar v), expOf (EInt 1))))), 
  expOf (EUFun(v, expOf (EVar v)))))

let etfun_test = 
  let v = Var.fresh "v" in
  expOf (EApp(expOf (ETFun(v, EIntTy, expOf (EVar v))), 
  expOf (EInt 1)))

let check_int_test = 
  expOf (ECheck (expOf (EInt 1), EIntTy))

let check_dyn_test = 
  let v = Var.fresh "v" in
  expOf (ECheck (expOf (EApp(expOf (EUFun (v, expOf (EVar v))), expOf (EInt 1))), EIntTy))

(* let check_fun_test = 
  let v = Var.fresh "v" in
  expOf (ECheck (expOf (EUFun (v, expOf (EVar v))), EFunTy (EIntTy, EIntTy))) *)

let if_int_equal_test =
  expOf (EIf (expOf (EInt 1), expOf (EInt 2), expOf (EInt 3)))

let if_dyn_equal_test = 
  let v = Var.fresh "v" in
  expOf (EIf (expOf (EApp(expOf (EUFun (v, expOf (EVar v))), expOf (EInt 1))), expOf (EInt 2), expOf (EInt 3)))

(* use parsefile to read in files in the external syntax *)
(* let parsefile f = fst (parse f) *)

let run_test test = 
  let translation, _ = translate (test) in
  (* print_endline ("original: " ^ (eToString test)); *)
  (* print_endline ("translation: " ^ (iToString translation)); *)
  let _, res = iInterp translation in
  (* print_endline ((eToString (eInterp test)) ^ " vs " ^ (iToString res)); *)
  assert_equal true (
  eiEqVal (eInterp test) res)

let translate_tests =
  "test suite for translate" >::: [
    "untyped_identity" >:: (fun _ -> 
        run_test test1);
    "increment_one_function" >:: (fun _ ->
        run_test op_test);
    "increment_one_int" >:: (fun _ ->
        run_test op_int_test);
    "typed_identity" >:: (fun _ ->
        run_test etfun_test);
    "typed_identity_fun" >:: (fun _ ->
        run_test typed_var_fun_test);
    "check_int" >:: (fun _ ->
        run_test check_int_test);
    "check_dyn" >:: (fun _ ->
        run_test check_dyn_test);
    (* "check_fun" >:: (fun _ ->
        run_test check_fun_test); *)
    "if_int_equal" >:: (fun _ ->
        run_test if_int_equal_test);
    "if_dyn_equal" >:: (fun _ ->
        run_test if_dyn_equal_test);
  ] 

let _ = print_endline "running tests"
  ; run_test_tt_main translate_tests
