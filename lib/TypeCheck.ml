open Ast

exception UnboundVariable of string
exception TypeError of string
exception Impossible

(**************************)
(* External type checking *)
(**************************)

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
    | EUFun (x, e) -> isWF (Context.add x Val env) e
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
  | EUFun _ -> None

(**************************)
(* Internal type checking *)
(**************************)

type iEnv = iType Context.t
let rec iTypeOf (env: iEnv) (e: iExp) =
  match e with
  | IVar x -> begin
    match Context.find_opt x env with
      | None -> raise (UnboundVariable 
                ("unbound var " ^ Var.to_string x))
      | Some (t) -> t
  end
  | IInt _ -> IIntTy
  | IOp (Inc, e) -> 
    if iTypeOf env e = IIntTy then IIntTy 
    else raise (TypeError "increase applied to non-int")
  | IFun (x, ty, body) -> 
    let outputTy = iTypeOf (Context.add x ty env) body in
    IFunTy (ty, outputTy)
  | IApp (e1, e2) -> begin
    match iTypeOf env e1 with
    | IFunTy (t1, t2) -> if iTypeOf env e2 = t1 
      then t2 
      else raise (TypeError "incorrect input type for function")
    | _ ->  raise (TypeError "non-function applied")
  end
  | IIf (e1, e2, e3) -> begin
    if iTypeOf env e1 = IIntTy then (
      let branchTy = iTypeOf env e2 in
      if branchTy = iTypeOf env e3 then branchTy
      else raise (TypeError "branch types are not equal"))
    else raise (TypeError "incorrect type as condition of if")
  end
  | II2D (e1) -> begin
    match iTypeOf env e1 with
    | IIntTy -> IDynTy
    | _ -> raise (TypeError "int wrap on non-int")
  end
  | IF2D (e1) -> begin 
    match iTypeOf env e1 with
    | IFunTy (_, _) -> IDynTy
    | _ -> raise (TypeError "fun wrap on non-fun")
  end
  | ID2I (e1) -> begin
    match iTypeOf env e1 with 
    | IDynTy -> IIntTy
    | _ -> raise (TypeError "intQ on non-dyn")
  end
  | ID2F (e1) -> begin 
    match iTypeOf env e1 with 
    | IDynTy -> IFunTy (IDynTy, IDynTy)
    | _ -> raise (TypeError "funQ on non-dyn")
  end
