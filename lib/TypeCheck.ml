open Ast

exception UnboundVariable of string
exception TypeError of string
exception Impossible

let fresh_id = ref 0
let next_id () =
  let id = !fresh_id in
  incr fresh_id;
  id

let rec expToType (env: eEnv) (e: exp) : eType =
  match e.eExp with
  | EIntTypeExpr -> EIntType
  | EFunTypeExpr (e1, e2) -> EFunType (expToType env e1, expToType env e2)
  | EFinTypeExpr e -> begin
    match e.eExp with
    | EInt n when n > 0 -> EFinType n (* only allow positive integer literals for Fin types *)
    | _ -> raise (TypeError
      ("type error in finite type: expected a positive integer literal at " ^ (Span.show_span e.espan)))
  end
  | EArrTypeExpr (e1, e2) -> begin
    let t1 = expToType env e1 in
    match t1 with
    | EFinType _ -> EArrType (t1, expToType env e2)
    | _ -> raise (TypeError
      ("type error in array type: expected a Fin type for the domain but got " ^ (typeString t1) ^ " at " ^ (Span.show_span e.espan)))
  end
  | EVar v -> begin
    match Context.find_opt v env with
    | Some (ETypeType t) -> t
    | Some t -> raise (TypeError
      ("type error: expected a type expression but got a value binding of type " ^ (typeString t) ^ " for variable " ^ (Var.to_string v) ^ " at " ^ (Span.show_span e.espan)))
    | None -> raise (UnboundVariable (Var.to_string v))
  end
  | _ -> raise (TypeError
    ("type error: expected a type expression but got " ^ (eToString e) ^ " at " ^ (Span.show_span e.espan)))

let rec typeOf (env: eEnv) (effects: effect list) (e: exp) : eType =
  match e.eExp with
  | EVar v -> begin
    match Context.find_opt v env with
    | Some t -> t
    | None -> raise (UnboundVariable (Var.to_string v))
  end
  | EInt _ -> EIntType
  | EPlus (e1, e2) | EMinus (e1, e2) | ETimes (e1, e2) | EDiv (e1, e2) -> begin
    let t1 = typeOf env effects e1 in
    let t2 = typeOf env effects e2 in
    match t1, t2 with
    | EIntType, EIntType -> EIntType
    | _ -> raise (TypeError
      ("type error in arithmetic operation at " ^ (Span.show_span e.espan)))
  end
  | ETFun (v, e, body) ->
    let t = expToType env e in
    let env' = Context.add v t env in
    let tBody = typeOf env' effects body in
    EFunType (t, tBody)
  | EApp (e1, e2) -> begin
    let t1 = typeOf env effects e1 in
    let t2 = typeOf env effects e2 in
    match t1 with
    | EFunType (tArg, tRet) ->
      if tArg = t2 then tRet
      else raise (TypeError
        ("type error in application: expected argument of type " ^ (typeString tArg) ^ " but got " ^ (typeString t2) ^ " at " ^ (Span.show_span e.espan)))
    | _ -> raise (TypeError
      ("type error in application: expected a function at " ^ (Span.show_span e.espan)))
  end
  | EIf (e1, e2, e3) -> begin
    let t1 = typeOf env effects e1 in
    let t2 = typeOf env effects e2 in
    let t3 = typeOf env effects e3 in
    match t1 with
    | EIntType ->
      if t2 = t3 then t2
      else raise (TypeError
        ("type error in if branches: expected both branches to have the same type but got " ^ (typeString t2) ^ " and " ^ (typeString t3) ^ " at " ^ (Span.show_span e.espan)))
    | _ -> raise (TypeError
      ("type error in if condition: expected an int at " ^ (Span.show_span e.espan)))
  end
  | ELet (v, e1, e2) -> begin
    let t1 = typeOf env effects e1 in
    let env' = Context.add v t1 env in
    typeOf env' effects e2
  end
  | EFor (v, e1, e2) -> begin
    let t1 = expToType env e1 in
    match t1 with
    | EFinType _ ->
      let env' = Context.add v t1 env in
      let t2 = typeOf env' effects e2 in
      EArrType (t1, t2)
    | _ -> raise (TypeError
      ("type error in for loop: expected a Fin or Var of Fin type  " ^ (Span.show_span e.espan)))
  end
  | EArr _ -> raise Impossible (* not used by the programmer, only for internal representation of arrays *)
  | EArrIndex (e1, e2) -> begin
    let t1 = typeOf env effects e1 in
    let t2 = typeOf env effects e2 in
    match t1 with
    | EArrType (tIn, tOut) ->
      if tIn = t2 then tOut
      else raise (TypeError
        ("type error in array indexing: expected index of type " ^ (typeString tIn) ^ " but got " ^ (typeString t2) ^ " at " ^ (Span.show_span e.espan)))
    | _ -> raise (TypeError
      ("type error in array indexing: expected an array type at " ^ (Span.show_span e.espan)))
  end
  | EOrd e1 -> begin
    let t = typeOf env effects e1 in
    match t with
    | EFinType _ -> EIntType
    | _ -> raise (TypeError
      ("type error in ord: expected a Fin type but got " ^ (typeString t) ^ " at " ^ (Span.show_span e.espan)))
  end
  | EFromOrd (e1, e2) -> begin
    match e1.eExp with
    | EInt n -> begin (* only allow integer literals *)
      let t2 = typeOf env effects e2 in
      match t2 with
      | EFinType m | ETypeType (EFinType m) -> if n < m && n >= 0 then EFinType m
      else raise (TypeError ("type error in fromOrd: integer literal " ^ string_of_int n ^ " is out of bounds for the Fin type " ^ (typeString t2) ^ " at " ^ (Span.show_span e1.espan)))
      | _ -> raise (TypeError ("type error in fromOrd: expected a Fin type for the second argument but got " ^ (typeString t2) ^ " at " ^ (Span.show_span e2.espan)))
    end
    | _ -> raise (TypeError
      ("type error in fromOrd: expected an int literal for the first argument but got " ^ (eToString e1) ^ " at " ^ (Span.show_span e.espan)))
  end
  | ERunState (e1, ref, e2) -> begin
    let t1 = typeOf env effects e1 in
    match t1 with       (* run state can only use int types *)
    | EIntType -> begin
      let id = next_id () in
      let env' = Context.add ref (ERefType (id, EIntType)) env in
      let effects' = StateEffect id :: effects in
      let _ = typeOf env' effects' e2 in
      EIntType (* the result of runState is always an int since the state is an int *)
    end
    | _ -> raise (TypeError
      ("type error in runState: expected an int for the initial value but got " ^ (typeString t1) ^ " at " ^ (Span.show_span e.espan)))
  end
  | EGet ref -> begin
    let t = typeOf env effects ref in
    match t with
    | ERefType (id, refType) -> if List.mem (StateEffect id) effects then refType
    else raise (TypeError
      ("type error in get: reference " ^ (eToString ref) ^ " is not in scope for reading at " ^ (Span.show_span e.espan)))
    | _ -> raise (TypeError
      ("type error in get: expected a reference type for " ^ (eToString ref) ^ " but got " ^ (typeString t) ^ " at " ^ (Span.show_span e.espan)))
  end
  | EPut (ref, e1) -> begin
    let t = typeOf env effects ref in
    match t with
    | ERefType (id, refType) -> if List.mem (StateEffect id) effects then
      let t1 = typeOf env effects e1 in
      if t1 = refType then EUnitType
      else raise (TypeError
        ("type error in put: expected value of type " ^ (typeString refType) ^ " but got " ^ (typeString t1) ^ " at " ^ (Span.show_span e.espan)))
    else raise (TypeError
      ("type error in put: reference " ^ (eToString ref) ^ " is not in scope for writing at " ^ (Span.show_span e.espan)))
    | _ -> raise (TypeError
      ("type error in put: expected a reference type for " ^ (eToString ref) ^ " but got " ^ (typeString t) ^ " at " ^ (Span.show_span e.espan)))
  end
  | ERef _ -> raise Impossible (* not used by the programmer, only for internal representation of arrays *)
  | EIntTypeExpr | EFunTypeExpr _  | EFinTypeExpr _ | EArrTypeExpr _ | EUnit -> ETypeType (expToType env e)

let typeCheck (e:exp) : eType =
  typeOf Context.empty [] e