type id = string

type op =
  | Plus
  | Mult
  | Lt

type exp =
  | EVar of id
  | EInt of int
  | EBool of bool
  | EBinOp of op * exp * exp
  | EIfThenElse of exp * exp * exp

let string_of_op = function
  | Plus -> "+"
  | Mult -> "*"
  | Lt -> "<"

let rec string_of_exp = function
  | EVar x -> x
  | EInt i -> string_of_int i
  | EBool b -> string_of_bool b
  | EBinOp (op, e1, e2) ->
      Printf.sprintf "(%s %s %s)" (string_of_op op) (string_of_exp e1) (string_of_exp e2)
  | EIfThenElse (e1, e2, e3) ->
      Printf.sprintf "(if %s %s %s)" (string_of_exp e1) (string_of_exp e2) (string_of_exp e3)
