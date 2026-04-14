(*
Name: Jenny Fan
NetID: jf8083

Name: Spencer Doyle
NetID: sd4151
*)

open Ast


let rec toDyn (t: iType)= 
  match t with
  | IIntTy -> 
    let x = Var.fresh "x" in
    IFun (x, IIntTy, II2D (IVar x))
  | IFunTy (t1, t2) -> 
    let x = Var.fresh "x" in
    let y = Var.fresh "y" in
    IFun (x, IFunTy(t1, t2), IF2D (IFun (y, IDynTy, IApp (toDyn(t2), IApp (IVar x, IApp (fromDyn(t1), IVar y))))))
  | IDynTy -> 
    let x = Var.fresh "x" in
    IFun (x, IDynTy, IVar x)
  
and fromDyn (t: iType)= 
  match t with
  | IIntTy -> 
    let x = Var.fresh "x" in
    IFun (x, IDynTy, ID2I (IVar x))
  | IFunTy (t1, t2) -> 
    let x = Var.fresh "x" in
    let y = Var.fresh "y" in
    IFun (x, IDynTy, IFun (y, t1, IApp (fromDyn(t2), IApp (ID2F (IVar x), IApp (toDyn(t1), IVar y)))))
  | IDynTy -> 
    let x = Var.fresh "x" in
    IFun (x, IDynTy, IVar x)
  

(* produces equivalent expression with type Dyn *)
let asDyn (e: iExp) (t: iType) : iExp =
  match t with 
  | IDynTy -> e
  | _ -> IApp (toDyn(t), e)

(* produces equivalent expression with type Int *)
let asInt (e: iExp) (t: iType) : iExp =
  match t with 
  | IIntTy -> e
  | _ -> ID2I(asDyn e t)

(* produces equivalent expression with type Dyn -> Dyn *)
let asDynFun (e: iExp) (t: iType) : iExp =
  match t with 
  | IFunTy(IDynTy, IDynTy) -> e
  | _ -> ID2F(asDyn e t)

let rec toiType (t: eType) : iType =
  match t with
  | EIntTy -> IIntTy
  | EFunTy (t1, t2) -> IFunTy (toiType t1, toiType t2)

(* This is the main recursive part of the translation *)
let rec translateR (c : judgement Context.t) (e : exp) : (iExp * iType) = 
  match e.eExp with
  | EVar x -> (
    let t = Context.find x c in
    match t with
    | Val -> (IVar x, IDynTy)
    | Type t' -> 
      match t' with
      | EIntTy -> (IVar x, IIntTy)
      | EFunTy (t1, t2) -> (IVar x, IFunTy (toiType t1, toiType t2))
  )
  | EInt i -> (IInt i, IIntTy)
  | EOp (o, e) -> (
    match o with 
    | Inc ->
      let (e', t) = translateR c e in
      match t with 
      | IIntTy -> (IOp (Inc, e'), IIntTy)
      | IDynTy -> (IOp (Inc, asInt e' IDynTy), IIntTy)
      | IFunTy _ -> (IOp (Inc, asInt e' t), IIntTy)
    )
  | ETFun (x, ty, body) -> 
    let c' = Context.add x (Type ty) c in 
    let (body', bodyTy) = translateR c' body in
    let t' = toiType ty in
    (IFun (x, t', body'), IFunTy (t', bodyTy))
  | EUFun (x, body) ->
    let c' = Context.add x Val c in
    let (body', bodyTy) = translateR c' body in
    if bodyTy = IDynTy then (IFun (x, IDynTy, body'), IFunTy (IDynTy, IDynTy))
    else (IF2D (IFun (x, IDynTy, asDyn body' bodyTy)), IDynTy)
  | EApp (x1, x2) ->
    let (x1', t) = translateR c x1 in
    let (x2', t3) = translateR c x2 in
    (match t with
     | IFunTy (t1, t2) ->
      (IApp (x1', IApp ((fromDyn t1), (asDyn x2' t3))), t2)
     | IDynTy ->
      (IApp (asDynFun x1' t, asDyn x2' t3), IDynTy)
     | IIntTy ->
      (IApp (asDynFun (asDyn x1' t) IDynTy, asDyn x2' t3), IDynTy)
    )
  | ECheck (e1, t) -> (
    let (e1', t') = translateR c e1 in
    let t1 = toiType t in
    if (t' = t1) then (e1', t')
    else match t' with
     | IDynTy -> (IApp(fromDyn t1, e1'), t1)
     | _ -> (IApp(fromDyn t1, asDyn e1' t'), t1)
    )
  | EIf (e1, e2, e3) -> 
    let (e1', t1) = translateR c e1 in
    let (e2', t2) = translateR c e2 in
    let (e3', t3) = translateR c e3 in
    match t1 with
    | IIntTy ->
      if t2 = t3 then (IIf (e1', e2', e3'), t2)
      else (IIf (e1', asDyn e2' t2, asDyn e3' t3), IDynTy)
    | IDynTy ->
      if t2 = t3 then (IIf (asInt e1' IDynTy, e2', e3'), t2)
      else (IIf (asInt e1' IDynTy, asDyn e2' t2, asDyn e3' t3), IDynTy)
    | IFunTy _ ->
      if t2 = t3 then (IIf (asInt (asDyn e1' t1) IDynTy, e2', e3'), t2)
      else (IIf (asInt (asDyn e1' t1) IDynTy, asDyn e2' t2, asDyn e3' t3), IDynTy)
      
(* This is the top-level function *)
let translate (e: exp) : iExp * iType = 
  translateR Context.empty e
  