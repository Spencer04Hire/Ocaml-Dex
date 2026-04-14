type var = Var.t

type eType =
  | EIntTy
  | EFunTy of eType * eType

type op =
  | Inc
  (* | Dec *)

type eExp = (* use span to parse *)
  | EVar of var
  | EInt of int
  | EOp of op * exp
  (* lambdas (non-recursive functions) *)
  | ETFun of var * eType * exp
  | EUFun of var * exp 
  | EApp of exp * exp
  | ECheck of exp * eType
  | EIf of exp * exp * exp (* if 0, take first branch; else, take second*)
and exp = 
  { eExp : eExp
  ; espan : Span.t
}

let aexp e span = { eExp = e ; espan = span}
let expOf e = {eExp = e; espan = Span.default}

module Context = Map.Make(Var)
type context = eExp Context.t
type judgement = Val | Type of eType
type eEnv = judgement Context.t

let rec eToString (e: exp) =
  let rec typeString (t: eType) = 
    match t with
    | EIntTy -> "int"
    | EFunTy (t1, t2) -> "(" ^ typeString t1 ^ ") -> (" ^ typeString t2 ^ ")"
  in
  match e.eExp with
  | EVar x -> Var.to_string x
  | EInt i -> Int.to_string i
  | EOp (Inc, e1) -> "(Inc" ^ eToString e1 ^ ")"
  | ETFun (x, ty, exp) -> 
    "fun " ^ Var.to_string x ^ ": " ^ typeString ty ^ " is " ^ eToString exp ^ " end"
  | EUFun (x, exp) ->
    "fun " ^ Var.to_string x ^ " is " ^ eToString exp ^ " end"
  | EApp (e1, e2) ->
    "(" ^ eToString e1 ^ ") (" ^ eToString e2 ^ ")"
  | ECheck (e1, t) ->
    "check (" ^ eToString e1 ^ " as " ^ typeString t ^ ")"
  | EIf (e1, e2, e3) ->
    "if " ^ eToString e1 ^ "= 0 then \n" ^ eToString e2 
    ^ "\n else \n" ^ eToString e3 ^ "fi"

(*******************)
(* Internal syntax *)
(*******************)

type iType =
  | IIntTy
  | IFunTy of iType * iType
  | IDynTy

type iExp =
  | IVar of var
  | IInt of int
  | IOp of op * iExp
  | IFun of var * iType * iExp
  | IApp of iExp * iExp
  | IIf of iExp * iExp * iExp
  (* makes things INTO dyns *)
  | II2D of iExp 
  | IF2D of iExp
  (* "unwraps" OUT of dyns *)
  | ID2I of iExp
  | ID2F of iExp

let rec iToString (e: iExp) =
  let rec typeString (t: iType) = 
    match t with
    | IIntTy -> "int"
    | IFunTy (t1, t2) -> "(" ^ typeString t1 ^ ") -> (" ^ typeString t2 ^ ")"
    | IDynTy -> "dyn"
  in
  match e with
  | IVar v -> Var.to_string v
  | IInt i -> Int.to_string i
  | IOp (Inc, e1) -> "(Inc" ^ iToString e1 ^ ")"
  | IFun (x, ty, exp) -> 
    "fun " ^ Var.to_string x ^ ": " ^ typeString ty ^ " -> " ^ iToString exp
  | IApp (e1, e2) ->
    "(" ^ iToString e1 ^ ") (" ^ iToString e2 ^ ")"
  | IIf (e1, e2, e3) ->
    "if " ^ iToString e1 ^ "= 0 then {\n" ^ iToString e2 
    ^ "\n} else {\n" ^ iToString e3 ^ "}"
  | II2D (e1) -> "int2dyn(" ^ iToString e1 ^ ")"
  | IF2D (e1) -> "fun2dyn(" ^ iToString e1 ^ ")"
  | ID2I (e1) -> "dyn2int(" ^ iToString e1 ^ ")"
  | ID2F (e1) -> "dyn2fun(" ^ iToString e1 ^ ")"