type id = string

type op =
  | Plus
  | Mult
  | Lt
  | Cons

type exp =
  | EVar of id
  | EInt of int
  | EBool of bool
  | ENil
  | EBinOp of op * exp * exp
  | EIfThenElse of exp * exp * exp
  | ELet of id * exp * exp
  | EAbs of id * exp
  | EApp of exp * exp
  | ELetRec of id * exp * exp
  | EMatchWith of exp * exp * id * id * exp

type tyvar = int

type ty =
  | TInt
  | TBool
  | TVar of tyvar
  | TFun of ty * ty
  | TList of ty

type tysc = TScheme of tyvar list * ty
let tysc_of_ty ty = TScheme ([], ty)

module FTV = Set.Make(
  struct
    type t = tyvar
    let compare = compare
  end
)

let rec ftv = function
  | TInt
  | TBool -> FTV.empty
  | TVar tv -> FTV.singleton tv
  | TFun (t1, t2) -> FTV.union (ftv t1) (ftv t2)
  | TList t -> ftv t

let ftv_tysc (TScheme (vars, ty)) =
  FTV.diff (ftv ty) (FTV.of_list vars)

module Env = Map.Make(String)

let string_of_op = function
  | Plus -> "+"
  | Mult -> "*"
  | Lt -> "<"
  | Cons -> "::"

let rec string_of_exp = function
  | EVar x -> x
  | EInt i -> string_of_int i
  | EBool b -> string_of_bool b
  | ENil -> "()"
  | EBinOp (op, e1, e2) ->
      Printf.sprintf "(%s %s %s)" (string_of_op op) (string_of_exp e1) (string_of_exp e2)
  | EIfThenElse (e1, e2, e3) ->
      Printf.sprintf "(if %s %s %s)" (string_of_exp e1) (string_of_exp e2) (string_of_exp e3)
  | ELet (x, e1, e2) ->
      Printf.sprintf "(let %s %s %s)" x (string_of_exp e1) (string_of_exp e2)
  | EAbs (x, e) ->
      Printf.sprintf "(lambda %s %s)" x (string_of_exp e)
  | EApp (e1, e2) ->
      Printf.sprintf "(%s %s)" (string_of_exp e1) (string_of_exp e2)
  | ELetRec (f, e1, e2) ->
      Printf.sprintf "(let-rec %s %s %s)" f (string_of_exp e1) (string_of_exp e2)
  | EMatchWith (e1, enil, xcar, xcdr, econs) ->
      Printf.sprintf "(match %s (() %s)) ((%s . %s) %s)" (string_of_exp e1) (string_of_exp enil) xcar xcdr (string_of_exp econs)

let rec string_of_ty = function
  | TInt -> "int"
  | TBool -> "bool"
  | TVar tv -> "'t" ^ string_of_int tv
  | TFun (t1, t2) ->
      begin match t1 with
        | TFun _ ->
            Printf.sprintf "(%s) -> %s" (string_of_ty t1) (string_of_ty t2)
        | _ ->
            Printf.sprintf "%s -> %s" (string_of_ty t1) (string_of_ty t2)
      end
  | TList t ->
      begin match t with
        | TFun _ ->
            Printf.sprintf "(%s) list" (string_of_ty t)
        | _ -> string_of_ty t ^ " list"
      end
