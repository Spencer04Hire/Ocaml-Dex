type var = Var.t

type eType =
  | EIntType
  | EFunType of eType * eType

type op =
  | Inc
  (* | Dec *)

type eExp = (* use span to parse *)
  | EVar of var
  | EInt of int
  | EOp of op * exp
  (* lambdas (non-recursive functions) *)
  | ETFun of var * eType * exp
  | EApp of exp * exp
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
    | EIntType -> "int"
    | EFunType (t1, t2) -> "(" ^ typeString t1 ^ ") -> (" ^ typeString t2 ^ ")"
  in
  match e.eExp with
  | EVar x -> Var.to_string x
  | EInt i -> Int.to_string i
  | EOp (Inc, e1) -> "(Inc" ^ eToString e1 ^ ")"
  | ETFun (x, ty, exp) -> 
    "fun " ^ Var.to_string x ^ ": " ^ typeString ty ^ " is " ^ eToString exp ^ " end"
  | EApp (e1, e2) ->
    "(" ^ eToString e1 ^ ") (" ^ eToString e2 ^ ")"
  | EIf (e1, e2, e3) ->
    "if " ^ eToString e1 ^ "= 0 then \n" ^ eToString e2
    ^ "\n else \n" ^ eToString e3 ^ "fi"
