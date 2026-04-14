open Ast
exception UnboundVariable of string
exception Fail of string

(*******************************)
(* External Syntax Interpreter *)
(*******************************)

(* substitutes x with e1 in e2 *)
let rec eSubst (x: var) (e1: exp) (e2: exp) =
  match e2.eExp with
  | EVar v -> if v = x then e1 else e2
  | EInt _ -> e2
  | EOp (op, e) -> {eExp = EOp(op, eSubst x e1 e); espan = e2.espan}
  | ETFun (v, t, body) -> if v = x then e2 else 
    {eExp = ETFun(v, t, eSubst x e1 body); espan = e2.espan}
  | EUFun (v, body) -> if v = x then e2 else 
    {eExp = EUFun(v, eSubst x e1 body); espan = e2.espan}
  | EApp (f, e) ->
     {eExp = EApp(eSubst x e1 f, eSubst x e1 e); espan = e2.espan}
  | ECheck (e, t) ->
    {eExp = ECheck(eSubst x e1 e, t); espan = e2.espan}
  | EIf (cond, th, el) -> 
    {eExp = EIf(eSubst x e1 cond, eSubst x e1 th, eSubst x e1 el)
    ; espan = e2.espan}

let rec eInterp (e: exp) =
  match e.eExp with
  | EVar v -> raise (UnboundVariable (Var.to_string v))
  | EInt _ -> e
  | EOp (Inc, e) -> begin
    let interped = eInterp e in
    match interped.eExp with
    | EInt i -> {eExp = EInt (i+1); espan = interped.espan}
    | _ -> raise (Fail 
      ("inc applied to a non-integer value at " ^ (Span.show_span e.espan)))
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
    | EUFun (v, body) -> eInterp (eSubst v v2 body)
    | ETFun (v, t, body) -> 
      let newfun = expOf(EUFun(v, body)) in
      let newApplicand = expOf(ECheck(v2,t)) in
      eInterp ({eExp = EApp(newfun, newApplicand); espan = Span.default})
    | _ -> raise (Fail 
    ("application of non-function type at" ^ Span.show_span e.espan))
    end
  | ECheck (e1, t) -> begin
    let v1 = eInterp e1 in
    match v1.eExp with
    | EInt n -> if t = EIntTy then expOf (EInt n) else 
      raise (Fail ("function check applied to non function at" ^ Span.show_span e1.espan))
    | _ -> begin 
      match t with
      | EFunTy (t1, t2) -> 
        let x = Var.fresh "x" in
        let body = expOf (ECheck (expOf (EApp(v1, expOf(EVar x))), t2)) in
        eInterp (expOf(ETFun (x, t1, body)))
      | _ -> raise (Fail ("int check applied to non int at" ^ Span.show_span e1.espan))
    end
  end
  | ETFun _ | EUFun _ -> e

(*******************************)
(* Internal Syntax Interpreter *)
(*******************************)

let rec iSubst (x: var) (e1: iExp) (e2: iExp) =
  match e2 with
  | IVar y -> if x = y then e1 else e2
  | IInt _ -> e2
  | IOp (o, e) -> IOp(o, iSubst x e1 e)
  | IFun (v, t, body) -> if v = x then e2 else IFun (v, t, iSubst x e1 body)
  | IApp (e3, e4) -> IApp (iSubst x e1 e3, iSubst x e1 e4)
  | IIf (e3, e4, e5) -> IIf (iSubst x e1 e3, iSubst x e1 e4, iSubst x e1 e5)
  | II2D e -> II2D (iSubst x e1 e)
  | IF2D e -> IF2D (iSubst x e1 e)
  | ID2I e -> ID2I (iSubst x e1 e)
  | ID2F e -> ID2F (iSubst x e1 e)

let rec isResult (e: iExp) =
  match e with
  | IInt _ | IFun _ -> true
  | II2D e1 | IF2D e1 -> isResult e1
  | _ -> false

type cast_count = {i2d: int; f2d: int; d2i: int; d2f: int}

let combineCounts c1 c2 = 
  {i2d = c1.i2d + c2.i2d; f2d = c1.f2d + c2.f2d; 
  d2i= c1.d2i + c2.d2i; d2f= c1.d2f + c2.d2f}

let rec iInterpR (count: cast_count) (e: iExp) : cast_count * iExp=
  match e with
  | IVar x -> raise (UnboundVariable (Var.to_string x))
  | IInt i -> count, IInt i
  | IOp (Inc, e1) -> begin 
    match iInterpR count e1 with
     | count', IInt i -> count', IInt (i+1)
     | _ -> raise (Fail ("inc applied to non int"))
  end
  | IFun _ -> count, e
  | IApp (e1, e2) -> begin
    match (iInterpR count e1), (iInterpR count e2) with
     | (count', IFun (v, _, body)),( count'', v2) -> 
      iInterpR (combineCounts count' count'') (iSubst v v2 body)
     | _ -> raise (Fail ("bad function application"))
    end
  | IIf (cond, b1, b2) ->
    begin 
    match iInterpR count cond with
     | count', IInt i -> if i = 0 then iInterpR count' b1 else iInterpR count' b2
     | _ -> raise (Fail ("bad if condition"))
    end
  | II2D (e1) -> let count', v = (iInterpR count e1) in
    let rescount = {i2d = count'.i2d +1 ; f2d = count'.f2d; d2i = count'.d2i; d2f = count'.d2f} in
    rescount, II2D v
  | IF2D (e1) -> let count', v = (iInterpR count e1) in 
    let rescount = {i2d = count'.i2d; f2d = count'.f2d + 1; d2i = count'.d2i; d2f = count'.d2f} in
    rescount, IF2D v
  | ID2I (e1) -> begin
    match iInterpR count e1 with
    | count', II2D (v) -> if isResult v then 
      let rescount = {i2d = count'.i2d; f2d = count'.f2d; d2i = count'.d2i + 1; d2f = count'.d2f} in
      rescount, v 
      else raise (Fail "non-value in II2D")
    | _ -> raise (Fail "ID2I around non II2D")
  end
  | ID2F (e1) -> begin
    match iInterpR count e1 with
    | count', IF2D (v) -> if isResult v then 
      let rescount = {i2d = count'.i2d; f2d = count'.f2d; d2i = count'.d2i; d2f = count'.d2f+1} in
      rescount, v 
      else raise (Fail "non-value in II2D")
    | _ -> raise (Fail "ID2I around non II2D")
  end


let iInterp = iInterpR {i2d = 0; f2d = 0; d2i = 0; d2f = 0}