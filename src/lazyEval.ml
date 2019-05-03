open Syntax

type value =
  | VInt of int
  | VBool of bool
  | VProc of id * exp * thunk Env.t
  | VNil
  | VCons of thunk * thunk

and promise =
  | Promise of exp * thunk Env.t
  | Value of value

and thunk = promise ref

exception Error of string
let runtime_error s = raise (Error s)

let eval_binop op v1 v2 k = match (op, v1, v2) with
  | (Plus, VInt i1, VInt i2) -> k (VInt (i1 + i2))
  | (Mult, VInt i1, VInt i2) -> k (VInt (i1 * i2))
  | (Lt, VInt i1, VInt i2) -> k (VBool (i1 < i2))
  | _ -> runtime_error "binop"

let rec force t (k : value -> 'a) =
  let p = !t in match p with
  | Promise (e, env) ->
      begin match e with
      | EVar x ->
          begin match Env.find_opt x env with
            | Some t' -> force t' (fun v ->
                t := Value v;
                t' := Value v;
                k v
              )
            | None -> runtime_error ("Variable " ^ x ^ " is not bounded")
          end
      | EInt i ->
          t := Value (VInt i);
          k (VInt i)
      | EBool b ->
          t := Value (VBool b);
          k (VBool b)
      | ENil ->
          t := Value VNil;
          k VNil
      | EBinOp (op, e1, e2) ->
          eval env e1 (fun t1 ->
            eval env e2 (fun t2 ->
              match op with
                | Cons ->
                    t := Value (VCons (t1, t2));
                    k (VCons (t1, t2))
                | _ ->
                    force t1 (fun v1 ->
                      t1 := Value v1;
                      force t2 (fun v2 ->
                        t2 := Value v2;
                        eval_binop op v1 v2 (fun v ->
                          t := Value v;
                          k v
                        )
                      )
                    )
            )
          )
      | EIfThenElse (e1, e2, e3) ->
          eval env e1 (fun t1 ->
            force t1 (fun v1 ->
              t1 := Value v1;
              match v1 with
                | VBool true ->
                    eval env e2 (fun t2 ->
                      force t2 (fun v2 ->
                        t2 := Value v2;
                        k v2
                      )
                    )
                | VBool false ->
                    eval env e3 (fun t3 ->
                      force t3 (fun v3 ->
                        t3 := Value v3;
                        k v3
                      )
                    )
                | _ -> runtime_error "Condition must be boolean."
            )
          )
      | ELet (x, e1, e2) ->
          eval env e1 (fun t1 ->
            let env' = Env.add x t1 env in
            eval env' e2 (fun t2 ->
              force t2 (fun v2 ->
                t2 := Value v2;
                k v2
              )
            )
          )
      | EAbs (x, e) ->
          let proc = VProc (x, e, env) in
          t := Value proc;
          k proc
      | EApp (e1, e2) ->
          eval env e1 (fun t1 ->
            force t1 (fun v1 ->
              t1 := Value v1;
              match v1 with
              | VProc (x, e, envr) ->
                  eval env e2 (fun t2 ->
                    let env' = Env.add x t2 envr in
                    eval env' e (fun t ->
                      force t (fun v ->
                        t := Value v;
                        k v
                      )
                    )
                  )
              | _ -> runtime_error "Not a function."
            )
          )
      | ELetRec (f, e1, e2) ->
          let t1 = ref (Value (VInt 1)) in (* dummy *)
          let env' = Env.add f t1 env in
          eval env e1 (fun t1' ->
            t1 := !t1';
            eval env' e2 (fun t2 ->
              force t2 (fun v2 ->
                t2 := Value v2;
                k v2
              )
            )
          )
      | EMatchWith (e1, enil, xcar, xcdr, econs) ->
          eval env e1 (fun t1 ->
            force t1 (fun v1 ->
              t1 := Value v1;
              match v1 with
              | VNil ->
                  eval env enil (fun tnil ->
                    force tnil (fun vnil ->
                      tnil := Value vnil;
                      k vnil
                    )
                  )
              | VCons (tcar, tcdr) ->
                  let env' =
                    env
                    |> Env.add xcar tcar
                    |> Env.add xcdr tcdr
                  in
                  eval env' econs (fun tcons ->
                    force tcons (fun vcons ->
                      tcons := Value vcons;
                      k vcons
                    )
                  )
              | _ -> runtime_error "Not a list."
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
  | EInt i -> k (ref (Value (VInt i)))
  | EBool b -> k (ref (Value (VBool b)))
  | ENil -> k (ref (Value VNil))
  | EBinOp (op, e1, e2) ->
      k (ref (Promise (EBinOp (op, e1, e2), env)))
  | EIfThenElse (e1, e2, e3) ->
      k (ref (Promise (EIfThenElse (e1, e2, e3), env)))
  | ELet (x, e1, e2) ->
      eval env e1 (fun t1 ->
        let env' = Env.add x t1 env in
        eval env' e2 (fun t2 -> k t2)
      )
  | EAbs (x, e) ->
      k (ref (Value (VProc (x, e, env))))
  | EApp (e1, e2) ->
      k (ref (Promise (EApp (e1, e2), env)))
  | ELetRec (f, e1, e2) ->
      let t = ref (Value (VInt 1)) in (* dummy *)
      let env' = Env.add f t env in
      eval env' e1 (fun t1 ->
        t := !t1;
        eval env' e2 (fun t2 -> k t2)
      )
  | EMatchWith (e1, enil, xcar, xcdr, xcons) ->
      k (ref (Promise (EMatchWith (e1, enil, xcar, xcdr, xcons), env)))

let rec string_of_value = function
  | VInt i -> string_of_int i
  | VBool b -> string_of_bool b
  | VProc _ -> "<fun>"
  | VNil -> "[]"
  | VCons (t1, t2) ->
      force t1 (fun v1 ->
        force t2 (fun v2 ->
          t1 := Value v1;
          t2 := Value v2;
          match v1 with
            | VCons _ ->
                Printf.sprintf "(%s) :: %s" (string_of_value v1) (string_of_value v2)
            | _ ->
                Printf.sprintf "%s :: %s" (string_of_value v1) (string_of_value v2)
        )
      )

