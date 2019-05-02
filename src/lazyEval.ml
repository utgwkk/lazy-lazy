open Syntax

type value =
  | VInt of int
  | VBool of bool
  | VProc of id * exp * thunk Env.t ref

and thunk =
  | Promise of exp * thunk Env.t ref
  | Value of value

let string_of_value = function
  | VInt i -> string_of_int i
  | VBool b -> string_of_bool b
  | VProc (x, e, _) -> string_of_exp (EAbs (x, e))

exception Error of string
let runtime_error s = raise (Error s)

let eval_binop op v1 v2 k = match (op, v1, v2) with
  | (Plus, VInt i1, VInt i2) -> k (VInt (i1 + i2))
  | (Mult, VInt i1, VInt i2) -> k (VInt (i1 * i2))
  | (Lt, VInt i1, VInt i2) -> k (VBool (i1 < i2))
  | _ -> runtime_error "binop"

let rec force env t k = match t with
  | Promise (e, env) ->
      begin match e with
      | EVar x ->
          begin match Env.find_opt x !env with
            | Some t -> force env t (fun v -> k v)
            | None -> runtime_error ("Variable " ^ x ^ " is not bounded")
          end
      | EInt i -> k (VInt i)
      | EBool b -> k (VBool b)
      | EBinOp (op, e1, e2) ->
          eval !env e1 (fun t1 ->
            eval !env e2 (fun t2 ->
              force env t1 (fun v1 ->
                force env t2 (fun v2 ->
                  eval_binop op v1 v2 k
                )
              )
            )
          )
      | EIfThenElse (e1, e2, e3) ->
          eval !env e1 (fun t1 ->
            force env t1 (fun v1 ->
              match v1 with
                | VBool true ->
                    eval !env e2 (fun t2 ->
                      force env t2 (fun v2 -> k v2)
                    )
                | VBool false ->
                    eval !env e3 (fun t3 ->
                      force env t3 (fun v3 -> k v3)
                    )
                | _ -> runtime_error "Condition must be boolean."
            )
          )
      | ELet (x, e1, e2) ->
          eval !env e1 (fun t1 ->
            let env' = ref (Env.add x t1 !env) in
            eval !env' e2 (fun t2 ->
              force env' t2 (fun v2 -> k v2)
            )
          )
      | EAbs (x, e) -> k (VProc (x, e, env))
      | EApp (e1, e2) ->
          eval !env e1 (fun t1 ->
            force env t1 (function
              | VProc (x, e, envr) ->
                  eval !env e2 (fun t2 ->
                    let env' = ref (Env.add x t2 !envr) in
                    eval !env' e (fun t ->
                      force env' t (fun v -> k v)
                    )
                  )
              | _ -> runtime_error "Not a function."
            )
          )
      | ELetRec (f, e1, e2) ->
          eval !env e1 (fun t1 ->
            let envr = ref !env in
            let env' = Env.add f t1 !env in
            envr := env';
            eval env' e2 (fun t2 ->
              force envr t2 (fun v2 -> k v2)
            )
          )
    end
  | Value v -> k v

and eval env exp k = match exp with
  | EVar x ->
      begin match Env.find_opt x env with
        | Some t -> k t
        | None -> runtime_error ("Variable " ^ x ^ " is not bounded")
      end
  | EInt i -> k (Value (VInt i))
  | EBool b -> k (Value (VBool b))
  | EBinOp (op, e1, e2) ->
      k (Promise (EBinOp (op, e1, e2), ref env))
  | EIfThenElse (e1, e2, e3) ->
      k (Promise (EIfThenElse (e1, e2, e3), ref env))
  | ELet (x, e1, e2) ->
      eval env e1 (fun t1 ->
        let env' = Env.add x t1 env in
        k (Promise (e2, ref env'))
      )
  | EAbs (x, e) ->
      k (Value (VProc (x, e, ref env)))
  | EApp (e1, e2) ->
      k (Promise (EApp (e1, e2), ref env))
  | ELetRec (f, e1, e2) ->
      let envr = ref env in
      let t = Promise (e1, envr) in
      let env' = Env.add f t env in
      envr := env';
      k (Promise (e2, envr))
