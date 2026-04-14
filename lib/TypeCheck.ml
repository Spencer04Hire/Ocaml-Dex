open Ast

exception UnboundVariable of string
exception TypeError of string
exception Impossible

let rec equalTypes (t1:eType) (t2: eType) : bool =
  match t1, t2 with
  | EIntTy, EIntTy -> true
  | EFunTy (t11, t12), EFunTy (t21, t22) -> equalTypes t11 t21 && equalTypes t12 t22
  | _, _ -> false

let rec isWF (env: eEnv) (e: exp) : unit = 
  let (tau: eType option) = typeOf env e in
  match tau with
  | Some _ -> ()
  | None -> begin
    match e.eExp with
    | EVar x -> begin
      match Context.find_opt x env with
      | None -> raise (UnboundVariable 
                ("unbound var " ^ Var.to_string x ^ " on line" ^ Span.show_span e.espan))
      | Some Val -> ()
      | _ -> raise Impossible
    end
    | EIf (e1, e2, e3) -> isWF env e1; isWF env e2; isWF env e3
    | EApp (e1, e2) ->isWF env e1; isWF env e2
    | _ -> 
      raise (TypeError 
        ("expression on line" ^ Span.show_span e.espan ^ "is not wellformed"))
  end
and typeOf (env: eEnv) (e: exp) = 
  match e.eExp with
  | EVar x -> begin
      match Context.find_opt x env with
      | None -> raise (UnboundVariable 
                ("unbound var" ^ Var.to_string x ^" on line" ^ Span.show_span e.espan))
      | Some (Type t) -> Some t
      | Some Val -> None
  end
  | EInt _ -> Some EIntTy
  | EOp (_, e1) -> isWF env e1; Some EIntTy
  | EIf(e1, e2, e3) -> begin 
    isWF env e1; 
    match typeOf env e2, typeOf env e3 with
    | None, _ | _, None -> None
    | Some x, Some y -> if equalTypes x y then Some x else None
    end
  | EApp (e1, e2) -> begin 
    match typeOf env e1 with
    | Some EFunTy (_, t2) -> isWF env e2; Some t2
    | _ -> None
  end
  | ETFun (x, t, e1) -> begin
    match typeOf (Context.add x (Type t) env) e1 with
    | Some t2 -> Some (EFunTy (t,t2))
    | _ -> None
  end
  | ECheck (e1, t) -> begin
      isWF env e1; Some t
  end
