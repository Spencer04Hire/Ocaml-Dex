open Ast
exception UnboundVariable of string
exception Fail of string

(* substitutes x with e1 in e2 *)
let rec eSubst (x: var) (e1: exp) (e2: exp) =
  match e2.eExp with
  | EVar v -> if v = x then e1 else e2
  | EInt _ -> e2
  | EPlus (e21, e22) -> 
    {eExp = EPlus(eSubst x e1 e21, eSubst x e1 e22); espan = e2.espan}
  | EMinus (e21, e22) ->
    {eExp = EMinus(eSubst x e1 e21, eSubst x e1 e22); espan = e2.espan}
  | ETFun (v, t, body) -> if v = x then e2 else 
    {eExp = ETFun(v, t, eSubst x e1 body); espan = e2.espan}
  | EApp (f, e) ->
     {eExp = EApp(eSubst x e1 f, eSubst x e1 e); espan = e2.espan}
  | EIf (cond, th, el) -> 
    {eExp = EIf(eSubst x e1 cond, eSubst x e1 th, eSubst x e1 el)
    ; espan = e2.espan}
  | ELet (v, e1', e2') -> if v = x then e2 else
    {eExp = ELet(v, eSubst x e1 e1', eSubst x e1 e2'); espan = e2.espan}
  | EFin e -> {eExp = EFin (e); espan = e2.espan} (* don't substitute since Fin only takes in int literals *)

let rec eInterp (e: exp) =
  match e.eExp with
  | EVar v -> raise (UnboundVariable (Var.to_string v))
  | EInt _ -> e
  | EPlus (e1, e2) -> begin
    match (eInterp e1).eExp, (eInterp e2).eExp with
    | EInt i1, EInt i2 -> aexp (EInt (i1 + i2)) e.espan
    | _ -> raise (Fail
      ("addition of non-integer type at " ^ (Span.show_span e.espan)))
  end
  | EMinus (e1, e2) -> begin
    match (eInterp e1).eExp, (eInterp e2).eExp with
    | EInt i1, EInt i2 -> aexp (EInt (i1 - i2)) e.espan
    | _ -> raise (Fail
      ("subtraction of non-integer type at " ^ (Span.show_span e.espan)))
  end
  | EIf (e1, e2, e3) -> begin
    match (eInterp e1).eExp with
    | EInt 0 -> eInterp e2
    | EInt _ -> eInterp e3
    | _ -> raise (Fail 
      ("condition of if at " ^ (Span.show_span e.espan) ^ "is not an int"))
  end
  | EApp (e1, e2) -> begin
    let v2 = eInterp e2 in
    match (eInterp e1).eExp with
    | ETFun (v, _, body) ->
      eInterp (eSubst v v2 body)
    | _ -> raise (Fail
    ("application of non-function type at" ^ Span.show_span e.espan))
    end
  | ETFun _ -> e
  | ELet (v, e1, e2) -> eInterp (eSubst v (eInterp e1) e2)
  | EFin _ -> e
