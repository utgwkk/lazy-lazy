type id = string

let ignore_id = "_"

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
  | EUndefined
  | EBinOp of op * exp * exp
  | EIfThenElse of exp * exp * exp
  | ELet of id * exp * exp
  | EAbs of id * exp
  | EApp of exp * exp
  | ELetRec of id * exp * exp
  | EMatchWith of exp * (exp * exp) list
  | ETuple of exp list

type tyvar = int

type ty =
  | TInt
  | TBool
  | TVar of tyvar
  | TFun of ty * ty
  | TList of ty
  | TTuple of ty list

type tysc = TScheme of tyvar list * ty
let tysc_of_ty ty = TScheme ([], ty)

module TV = Set.Make(
  struct
    type t = tyvar
    let compare = compare
  end
)

let rec ftv = function
  | TInt
  | TBool -> TV.empty
  | TVar tv -> TV.singleton tv
  | TFun (t1, t2) -> TV.union (ftv t1) (ftv t2)
  | TList t -> ftv t
  | TTuple ts ->
      ts
      |> List.map ftv
      |> List.fold_left TV.union TV.empty

let ftv_tysc (TScheme (vars, ty)) =
  TV.diff (ftv ty) (TV.of_list vars)

module Env = Map.Make(String)

let string_of_op = function
  | Plus -> "+"
  | Mult -> "*"
  | Lt -> "<"
  | Cons -> "cons"

let rec string_of_exp = function
  | EVar x -> x
  | EInt i -> string_of_int i
  | EBool b -> string_of_bool b
  | ENil -> "()"
  | EUndefined -> "undefined"
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
  | EMatchWith (e, guards) ->
      let guards' =
        guards
        |> List.map (fun (p, e) -> Printf.sprintf "(%s %s)" (string_of_exp p) (string_of_exp e))
        |> String.concat " "
      in
      Printf.sprintf "(match %s %s)" (string_of_exp e) guards'
  | ETuple es ->
      let es' =
        es
        |> List.map string_of_exp
        |> String.concat " "
      in
      Printf.sprintf "(tuple (%s))" es'

let large_ty = function
  | TInt
  | TBool
  | TVar _
  | TList _ -> false
  | TFun _ -> true
  | TTuple _ -> true

let rec string_of_ty = function
  | TInt -> "int"
  | TBool -> "bool"
  | TVar tv -> "'t" ^ string_of_int tv
  | TFun (t1, t2) ->
      if large_ty t1 then
        Printf.sprintf "(%s) -> %s" (string_of_ty t1) (string_of_ty t2)
      else
        Printf.sprintf "%s -> %s" (string_of_ty t1) (string_of_ty t2)
  | TList t ->
      if large_ty t then
        Printf.sprintf "(%s) list" (string_of_ty t)
      else string_of_ty t ^ " list"
  | TTuple ts ->
      ts
      |> List.map (fun t ->
          if large_ty t then "(" ^ string_of_ty t ^ ")"
          else string_of_ty t
        )
      |> String.concat " * "

let pp_ty ty =
  let rec tvs = function
    | TInt
    | TBool -> TV.empty
    | TVar tv -> TV.singleton tv
    | TFun (t1, t2) -> TV.union (tvs t1) (tvs t2)
    | TList t -> tvs t
    | TTuple ts ->
        List.map tvs ts
        |> List.fold_left TV.union TV.empty
  in
  let tv_map =
    tvs ty
    |> TV.elements
    |> List.mapi (fun i tv -> (tv, i + 1))
  in
  let rec string_of_ty = function
    | TInt -> "int"
    | TBool -> "bool"
    | TVar tv -> "'t" ^ string_of_int (List.assoc tv tv_map)
    | TFun (t1, t2) ->
        if large_ty t1 then
          Printf.sprintf "(%s) -> %s" (string_of_ty t1) (string_of_ty t2)
        else
          Printf.sprintf "%s -> %s" (string_of_ty t1) (string_of_ty t2)
    | TList t ->
        if large_ty t then
          Printf.sprintf "(%s) list" (string_of_ty t)
        else string_of_ty t ^ " list"
    | TTuple ts ->
        ts
        |> List.map (fun t ->
            if large_ty t then "(" ^ string_of_ty t ^ ")"
            else string_of_ty t
          )
        |> String.concat " * "
  in string_of_ty ty
