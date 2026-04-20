type var = Var.t

type effect =
  | Pure
  | StateEffect of int

type eType =
  | EIntType
  | EFunType of eType * eType
  | EFinType of int
  | ETypeType of eType        (* for variables that are a type *)
  | EArrType of eType * eType (* Fin n => Type *)
  | ERefType of int * eType
  | EUnitType

type eExp = (* use span to parse *)
  | EVar of var
  | EInt of int
  | EPlus of exp * exp
  | EMinus of exp * exp
  | ETimes of exp * exp
  | EDiv of exp * exp         (* integer division *)
  (* lambdas (non-recursive functions) *)
  | ETFun of var * exp * exp
  | EApp of exp * exp
  | EIf of exp * exp * exp    (* if 0, take first branch; else, take second*)
  | ELet of var * exp * exp   (* let x = e1 in e2 *)
  | EFor of var * exp * exp   (* for x:Fin n. e *)
  | EArr of exp list          (* [e1; e2; e3], not used by the programmer *)
  | EArrIndex of exp * exp    (* e1[e2] *)
  | EOrd of exp               (* ord e *)
  | EFromOrd of exp * exp     (* e1 @ e2 *)
  (* ref expressions *)
  | ERunState of exp * var * exp
  | EGet of exp
  | EPut of exp * exp
  | ERef of int               (* only used for internal representation *)
  (* type constructors *)
  | EIntTypeExpr
  | EFunTypeExpr of exp * exp
  | EFinTypeExpr of exp           (* Fin 5 *)
  | EArrTypeExpr of exp * exp
  | EUnit
and exp =
  { eExp : eExp
  ; espan : Span.t
}

let aexp e span = { eExp = e ; espan = span}
let expOf e = {eExp = e; espan = Span.default}

module Context = Map.Make(Var)
type eEnv = eType Context.t

let rec typeString (t: eType) =
  match t with
  | EIntType -> "int"
  | EFinType n -> "Fin(" ^ Int.to_string n ^ ")"
  | EFunType (e1, e2) -> "(" ^ typeString e1 ^ ") -> (" ^ typeString e2 ^ ")"
  | ETypeType t -> "Type(" ^ typeString t ^ ")"
  | EArrType (e1, e2) -> "[" ^ typeString e1 ^ "] => " ^ typeString e2
  | ERefType (n, t) -> "Ref(" ^ Int.to_string n ^ ", " ^ typeString t ^ ")"
  | EUnitType -> "Unit"

let rec eToString (e: exp) =
  match e.eExp with
  | EVar x -> Var.to_string x
  | EInt i -> Int.to_string i
  | EPlus (e1, e2) -> "(" ^ eToString e1 ^ " + " ^ eToString e2 ^ ")"
  | EMinus (e1, e2) -> "(" ^ eToString e1 ^ " - " ^ eToString e2 ^ ")"
  | ETimes (e1, e2) -> "(" ^ eToString e1 ^ " * " ^ eToString e2 ^ ")"
  | EDiv (e1, e2) -> "(" ^ eToString e1 ^ " / " ^ eToString e2 ^ ")"
  | ETFun (x, ty, exp) -> 
    "fun " ^ Var.to_string x ^ ": " ^ eToString ty ^ " is " ^ eToString exp ^ " end"
  | EApp (e1, e2) ->
    "(" ^ eToString e1 ^ ") (" ^ eToString e2 ^ ")"
  | EIf (e1, e2, e3) ->
    "if " ^ eToString e1 ^ "= 0 then \n" ^ eToString e2
    ^ "\n else \n" ^ eToString e3 ^ "fi"
  | ELet (x, e1, e2) ->
    "let " ^ Var.to_string x ^ " = " ^ eToString e1 ^ " in \n" ^ eToString e2
  | EFor (x, e1, e2) -> "for " ^ Var.to_string x ^ ": " ^ eToString e1 ^ ". " ^ eToString e2
  | EArr es -> "[ " ^ String.concat "; " (List.map eToString es) ^ " ]"
  | EArrIndex (e1, e2) -> eToString e1 ^ "[" ^ eToString e2 ^ "]"
  | EOrd e -> "ord(" ^ eToString e ^ ")"
  | EFromOrd (e1, e2) -> eToString e1 ^ " @ " ^ eToString e2
  (* ref expressions *)
  | ERunState (e, v, body) -> "runState(" ^ eToString e ^ ", " ^ Var.to_string v ^ ". " ^ eToString body ^ ")"
  | EGet e1 -> "get(" ^ eToString e1 ^ ")"
  | EPut (e1, e2) -> "put(" ^ eToString e1 ^ ", " ^ eToString e2 ^ ")"
  | ERef n -> "ref(" ^ Int.to_string n ^ ")"
  (* type expressions *)
  | EIntTypeExpr -> "int"
  | EFinTypeExpr e -> "Fin(" ^ eToString e ^ ")"
  | EFunTypeExpr (e1, e2) -> "(" ^ eToString e1 ^ ") -> (" ^ eToString e2 ^ ")"
  | EArrTypeExpr (e1, e2) -> "[" ^ eToString e1 ^ "] => " ^ eToString e2
  | EUnit -> "Unit"