open Ast
exception Impossible
exception RuntimeError of string

let fresh_id = ref 0
let next_id () =
  let id = !fresh_id in
  incr fresh_id;
  id

(* substitutes x with e1 in e2 *)
let rec eSubst (x: var) (e1: exp) (e2: exp) =
  match e2.eExp with
  | EVar v -> if v = x then e1 else e2
  | EInt _ -> e2
  | EPlus (e21, e22) -> 
    {eExp = EPlus(eSubst x e1 e21, eSubst x e1 e22); espan = e2.espan}
  | EMinus (e21, e22) ->
    {eExp = EMinus(eSubst x e1 e21, eSubst x e1 e22); espan = e2.espan}
  | ETimes (e21, e22) ->
    {eExp = ETimes(eSubst x e1 e21, eSubst x e1 e22); espan = e2.espan}
  | EDiv (e21, e22) ->
    {eExp = EDiv(eSubst x e1 e21, eSubst x e1 e22); espan = e2.espan}
  | ETFun (v, t, body) ->
    let t' = eSubst x e1 t in
    if v = x then {eExp = ETFun(v, t', body); espan = e2.espan}
    else {eExp = ETFun(v, t', eSubst x e1 body); espan = e2.espan}
  | EApp (f, e) ->
     {eExp = EApp(eSubst x e1 f, eSubst x e1 e); espan = e2.espan}
  | EIf (cond, th, el) -> 
    {eExp = EIf(eSubst x e1 cond, eSubst x e1 th, eSubst x e1 el)
    ; espan = e2.espan}
  | ELet (v, e1', e2') ->
    let e1'' = eSubst x e1 e1' in
    if v = x then {eExp = ELet(v, e1'', e2'); espan = e2.espan}
    else {eExp = ELet(v, e1'', eSubst x e1 e2'); espan = e2.espan}
  | EFor (v, e1', e2') ->
    let e1'' = eSubst x e1 e1' in
    if v = x then {eExp = EFor(v, e1'', e2'); espan = e2.espan}
    else {eExp = EFor(v, e1'', eSubst x e1 e2'); espan = e2.espan}
  | EArr _ -> e2 (* don't substitute inside arrays since we only have evaluated expressions in arrays *)
  | EArrIndex (e1', e2') ->
    let e1'' = eSubst x e1 e1' in
    let e2'' = eSubst x e1 e2' in
    {eExp = EArrIndex (e1'', e2''); espan = e2.espan}
  | EOrd e -> {eExp = EOrd (eSubst x e1 e); espan = e2.espan}
  | EFromOrd (e1', e2') ->
    let e2'' = eSubst x e1 e2' in
    {eExp = EFromOrd (e1', e2''); espan = e2.espan} (* fromOrd can only be applied on integer literals so don't substitute *)
  | ERunState (e1', v, e2') ->
    let e1'' = eSubst x e1 e1' in
    if v = x then {eExp = ERunState(e1'', v, e2'); espan = e2.espan}
    else {eExp = ERunState(e1'', v, eSubst x e1 e2'); espan = e2.espan}
  | EGet e1' ->
    let e1'' = eSubst x e1 e1' in
    {eExp = EGet e1''; espan = e2.espan}
  | EPut (e1', e2')->
    let e1'' = eSubst x e1 e1' in
    let e2'' = eSubst x e1 e2' in
    {eExp = EPut (e1'', e2''); espan = e2.espan}
  | ERef _ -> e2
  | EIntTypeExpr -> e2
  | EFunTypeExpr (e1', e2') ->
    let e1'' = eSubst x e1 e1' in
    let e2'' = eSubst x e1 e2' in
    {eExp = EFunTypeExpr (e1'', e2''); espan = e2.espan}
  | EFinTypeExpr _ -> e2 (* Fin types can only contain integer literals (no variables) so don't substitute *)
  | EArrTypeExpr (e1', e2') ->
    let e1'' = eSubst x e1 e1' in
    let e2'' = eSubst x e1 e2' in
    {eExp = EArrTypeExpr (e1'', e2''); espan = e2.espan}
  | EUnit -> e2

(* Impossible indicates that with type checking, we should never reach this case *)
let rec eInterp (env: (int, int) Hashtbl.t) (e: exp) =
  match e.eExp with
  | EVar _ -> raise Impossible
  | EInt _ -> e
  | EPlus (e1, e2) -> begin
    match (eInterp env e1).eExp, (eInterp env e2).eExp with
    | EInt i1, EInt i2 -> aexp (EInt (i1 + i2)) e.espan
    | _ -> raise Impossible
  end
  | EMinus (e1, e2) -> begin
    match (eInterp env e1).eExp, (eInterp env e2).eExp with
    | EInt i1, EInt i2 -> aexp (EInt (i1 - i2)) e.espan
    | _ -> raise Impossible
  end
  | ETimes (e1, e2) -> begin
    match (eInterp env e1).eExp, (eInterp env e2).eExp with
    | EInt i1, EInt i2 -> aexp (EInt (i1 * i2)) e.espan
    | _ -> raise Impossible
  end
  | EDiv (e1, e2) -> begin
    match (eInterp env e1).eExp, (eInterp env e2).eExp with
    | EInt i1, EInt i2 -> if i2 = 0 then raise (RuntimeError ("Division by zero at " ^ (Span.show_span e.espan)))
    else aexp (EInt (i1 / i2)) e.espan
    | _ -> raise Impossible
  end
  | ETFun _ -> e
  | EApp (e1, e2) -> begin
    let v2 = eInterp env e2 in
    match (eInterp env e1).eExp with
    | ETFun (v, _, body) ->
      eInterp env (eSubst v v2 body)
    | _ -> raise Impossible
  end
  | EIf (e1, e2, e3) -> begin
    match (eInterp env e1).eExp with
    | EInt 0 -> eInterp env e2
    | EInt _ -> eInterp env e3
    | _ -> raise Impossible
  end
  | ELet (v, e1, e2) -> eInterp env (eSubst v (eInterp env e1) e2)
  | EFor (v, e1, e2) -> begin
    match (eInterp env e1).eExp with
    | EFinTypeExpr inner -> begin
      match inner.eExp with
      | EInt n ->
        let elements = List.init n (fun i ->
          let index = aexp (EInt i) e.espan in
          eInterp env (eSubst v index e2)
        ) in
        aexp (EArr elements) e.espan
      | _ -> raise Impossible
    end
    | _ -> raise Impossible
  end
  | EArr _ -> e
  | EArrIndex (e1, e2) -> begin
    let arr = eInterp env e1 in
    let index = eInterp env e2 in
    match arr.eExp, index.eExp with
    | EArr elements, EInt i -> List.nth elements i
    | _ -> raise Impossible
  end
  | EOrd e1 -> begin
    match (eInterp env e1).eExp with
    | EInt n -> aexp (EInt n) e.espan
    | _ -> raise Impossible
  end
  | EFromOrd (e1, _) -> begin
    match e1.eExp with
    | EInt n -> aexp (EInt n) e.espan
    | _ -> raise Impossible
  end
  | ERunState (e1, v, e2) -> begin
    let initial = eInterp env e1 in
    match initial.eExp with
    | EInt n ->
      let id = next_id () in
      Hashtbl.add env id n;
      let _ = eInterp env (eSubst v (aexp (ERef id) e.espan) e2) in
      let result = Hashtbl.find env id in
      Hashtbl.remove env id;
      aexp (EInt result) e.espan
    | _ -> raise Impossible
  end
  | EGet e1 -> begin
    match e1.eExp with
    | ERef i -> aexp (EInt (Hashtbl.find env i)) e.espan
    | _ -> raise Impossible
  end
  | EPut (e1, e2) -> begin
    let e2' = eInterp env e2 in
    match e1.eExp, e2'.eExp with
    | ERef i, EInt n -> Hashtbl.replace env i n; aexp EUnit e.espan
    | _ -> raise Impossible
  end
  | ERef _ -> e
  | EIntTypeExpr | EFunTypeExpr _  | EFinTypeExpr _ | EArrTypeExpr _ | EUnit -> e

let interp (e: exp) =
  let env = Hashtbl.create 10 in
  eInterp env e