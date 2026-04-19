open Ast

exception UnboundVariable of string
exception TypeError of string
exception Impossible

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
      ("type error in array type: expected a Fin type for the domain but got " ^ (Ast.typeString t1) ^ " at " ^ (Span.show_span e.espan)))
  end
  | EVar v -> begin
    match Context.find_opt v env with
    | Some (ETypeType t) -> t
    | Some t -> raise (TypeError
      ("type error: expected a type expression but got a value binding of type " ^ (Ast.typeString t) ^ " for variable " ^ (Var.to_string v) ^ " at " ^ (Span.show_span e.espan)))
    | None -> raise (UnboundVariable (Var.to_string v))
  end
  | _ -> raise (TypeError
    ("type error: expected a type expression but got " ^ (eToString e) ^ " at " ^ (Span.show_span e.espan)))

let rec typeOf (env: eEnv) (e: exp) : eType =
  match e.eExp with
  | EVar v -> begin
    match Context.find_opt v env with
    | Some t -> t
    | None -> raise (UnboundVariable (Var.to_string v))
  end
  | EInt _ -> EIntType
  | EPlus (e1, e2) | EMinus (e1, e2) -> begin
    let t1 = typeOf env e1 in
    let t2 = typeOf env e2 in
    match t1, t2 with
    | EIntType, EIntType -> EIntType
    | _ -> raise (TypeError
      ("type error in arithmetic operation at " ^ (Span.show_span e.espan)))
  end
  | ETFun (v, e, body) ->
    let t = expToType env e in
    let env' = Context.add v t env in
    let tBody = typeOf env' body in
    EFunType (t, tBody)
  | EApp (e1, e2) -> begin
    let t1 = typeOf env e1 in
    let t2 = typeOf env e2 in
    match t1 with
    | EFunType (tArg, tRet) ->
      if tArg = t2 then tRet
      else raise (TypeError
        ("type error in application: expected argument of type " ^ (Ast.typeString tArg) ^ " but got " ^ (Ast.typeString t2) ^ " at " ^ (Span.show_span e.espan)))
    | _ -> raise (TypeError
      ("type error in application: expected a function at " ^ (Span.show_span e.espan)))
  end
  | EIf (e1, e2, e3) -> begin
    let t1 = typeOf env e1 in
    let t2 = typeOf env e2 in
    let t3 = typeOf env e3 in
    match t1 with
    | EIntType ->
      if t2 = t3 then t2
      else raise (TypeError
        ("type error in if branches: expected both branches to have the same type but got " ^ (Ast.typeString t2) ^ " and " ^ (Ast.typeString t3) ^ " at " ^ (Span.show_span e.espan)))
    | _ -> raise (TypeError
      ("type error in if condition: expected an int at " ^ (Span.show_span e.espan)))
  end
  | ELet (v, e1, e2) -> begin
    let t1 = typeOf env e1 in
    let env' = Context.add v t1 env in
    typeOf env' e2
  end
  | EFor (v, e1, e2) -> begin
    let t1 = expToType env e1 in
    match t1 with
    | EFinType _ ->
      let env' = Context.add v t1 env in
      let t2 = typeOf env' e2 in
      EArrType (t1, t2)
    | _ -> raise (TypeError
      ("type error in for loop: expected a Fin or Var of Fin type  " ^ (Span.show_span e.espan)))
  end
  | EArr _ -> raise Impossible (* not used by the programmer, only for internal representation of arrays *)

  | EIntTypeExpr | EFunTypeExpr _  | EFinTypeExpr _ | EArrTypeExpr _ -> ETypeType (expToType env e)