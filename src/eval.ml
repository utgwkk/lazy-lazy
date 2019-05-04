open Syntax

type value =
  | VInt of int
  | VBool of bool
  | VProc of id * exp * value Env.t ref
  | VNil
  | VCons of value * value

let rec string_of_value = function
  | VInt i -> string_of_int i
  | VBool b -> string_of_bool b
  | VProc _ -> "<fun>"
  | VNil -> "[]"
  | VCons (v1, v2) ->
      match v1 with
        | VCons _ ->
            Printf.sprintf "(%s) :: %s" (string_of_value v1) (string_of_value v2)
        | _ ->
            Printf.sprintf "%s :: %s" (string_of_value v1) (string_of_value v2)

exception Error of string
let runtime_error s = raise (Error s)

let eval_binop op v1 v2 k = match (op, v1, v2) with
  | (Plus, VInt i1, VInt i2) -> k (VInt (i1 + i2))
  | (Mult, VInt i1, VInt i2) -> k (VInt (i1 * i2))
  | (Lt, VInt i1, VInt i2) -> k (VBool (i1 < i2))
  | (Cons, v1, v2) -> k (VCons (v1, v2))
  | _ -> runtime_error "binop"

let rec eval env exp k = match exp with
  | EVar x ->
      begin match Env.find_opt x env with
        | Some v -> k v
        | None -> runtime_error ("Variable " ^ x ^ " is not bounded")
      end
  | EInt i -> k (VInt i)
  | EBool b -> k (VBool b)
  | ENil -> k VNil
  | EUndefined -> runtime_error "undefined"
  | EBinOp (op, e1, e2) ->
      eval env e1 (fun v1 ->
        eval env e2 (fun v2 ->
          eval_binop op v1 v2 k
        )
      )
  | EIfThenElse (e1, e2, e3) ->
      eval env e1 (fun v1 ->
        match v1 with
          | VBool b ->
              if b then eval env e2 (fun v2 -> k v2)
              else eval env e3 (fun v3 -> k v3)
          | _ -> runtime_error "Condition must be boolean"
      )
  | ELet (x, e1, e2) ->
      eval env e1 (fun v1 ->
        let env' = Env.add x v1 env in
        eval env' e2 (fun v2 -> k v2)
      )
  | EAbs (x, e) ->
      k (VProc (x, e, ref env))
  | EApp (e1, e2) ->
      eval env e1 (fun v1 ->
        eval env e2 (fun v2 ->
          match v1 with
            | VProc (x, e, envr) ->
                let env' = Env.add x v2 !envr in
                eval env' e (fun v2 -> k v2)
            | _ -> runtime_error "Not a function"
        )
      )
  | ELetRec (f, EAbs (x, e1), e2) ->
      let envr = ref env in
      let env' = Env.add f (VProc (x, e1, envr)) env in
      envr := env';
      eval env' e2 (fun v2 -> k v2)
  | ELetRec _ -> runtime_error "Recursive variable is not allowed in strict mode"
  | EMatchWith (e1, enil, xcar, xcdr, econs) ->
      eval env e1 (function
        | VNil -> eval env enil (fun vnil -> k vnil)
        | VCons (vcar, vcdr) ->
            let env' =
              env
              |> Env.add xcar vcar
              |> Env.add xcdr vcdr
            in
            eval env' econs (fun vcons -> k vcons)
        | _ -> runtime_error "Not a list"
      )

let start exp =
  eval Env.empty exp (fun x -> x)
