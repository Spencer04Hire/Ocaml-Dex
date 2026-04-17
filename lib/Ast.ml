type var = Var.t

type eType =
  | EIntType
  | EFunType of eType * eType
  | ETypeType (* for variables that are a type *)


type eExp = (* use span to parse *)
  | EVar of var
  | EInt of int
  | EPlus of exp * exp
  | EMinus of exp * exp
  (* lambdas (non-recursive functions) *)
  | ETFun of var * eType * exp
  | EApp of exp * exp
  | EIf of exp * exp * exp (* if 0, take first branch; else, take second*)
  | ELet of var * exp * exp (* let x = e1 in e2 *)
  | EFin of exp
and exp = 
  { eExp : eExp
  ; espan : Span.t
}

let aexp e span = { eExp = e ; espan = span}
let expOf e = {eExp = e; espan = Span.default}

module Context = Map.Make(Var)
type judgement =
  | Type of eType
  | Fin of int
type eEnv = judgement Context.t

let rec typeString (t: eType) = 
  match t with
  | EIntType -> "int"
  | EFunType (t1, t2) -> "(" ^ typeString t1 ^ ") -> (" ^ typeString t2 ^ ")"
  | ETypeType -> "Type"

let rec eToString (e: exp) =
  match e.eExp with
  | EVar x -> Var.to_string x
  | EInt i -> Int.to_string i
  | EPlus (e1, e2) -> "(" ^ eToString e1 ^ " + " ^ eToString e2 ^ ")"
  | EMinus (e1, e2) -> "(" ^ eToString e1 ^ " - " ^ eToString e2 ^ ")"
  | ETFun (x, ty, exp) -> 
    "fun " ^ Var.to_string x ^ ": " ^ typeString ty ^ " is " ^ eToString exp ^ " end"
  | EApp (e1, e2) ->
    "(" ^ eToString e1 ^ ") (" ^ eToString e2 ^ ")"
  | EIf (e1, e2, e3) ->
    "if " ^ eToString e1 ^ "= 0 then \n" ^ eToString e2
    ^ "\n else \n" ^ eToString e3 ^ "fi"
  | ELet (x, e1, e2) ->
    "let " ^ Var.to_string x ^ " = " ^ eToString e1 ^ " in \n" ^ eToString e2
  | EFin e -> "Fin(" ^ eToString e ^ ")"
