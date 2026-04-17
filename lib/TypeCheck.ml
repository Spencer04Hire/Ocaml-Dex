open Ast

exception UnboundVariable of string
exception TypeError of string
exception Impossible

let rec typeOf (env: eEnv) (e: exp) : eType =
  match e.eExp with
  | EVar v -> begin
    match Context.find_opt v env with
    | Some (Type t) -> t
    | Some (Fin _) -> ETypeType
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
  | ETFun (v, t, body) -> 
    let env' = Context.add v (Type t) env in
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
    let binding = match e1.eExp with
    | EFin inner -> begin
      match inner.eExp with
      | EInt n -> Fin n
      | _ -> raise Impossible
    end
    | _ -> Type t1
    in
    let env' = Context.add v binding env in
    typeOf env' e2
  end
  | EFin e1 -> begin
    match e1.eExp with
    | EInt n ->
      if n > 0 then ETypeType
      else raise (TypeError
      ("type error in finite type: expected a positive integer but got " ^ (string_of_int n) ^ " at " ^ (Span.show_span e.espan)))
    | _ -> raise (TypeError
      ("type error in finite type: expected an integer literal at " ^ (Span.show_span e.espan)))
  end