open Syntax

type value =
  | VInt of int
  | VBool of bool
  | VProc of id * exp * thunk Env.t
  | VNil
  | VCons of thunk * thunk
  | VUndefined
  | VTuple of thunk list

and promise =
  | Promise of exp * thunk Env.t
  | Value of value
  | Exception

and thunk = promise ref

exception Error of string
let runtime_error s = raise (Error s)

let eval_binop op v1 v2 k = match (op, v1, v2) with
  | (Plus, VInt i1, VInt i2) -> k (VInt (i1 + i2))
  | (Mult, VInt i1, VInt i2) -> k (VInt (i1 * i2))
  | (Lt, VInt i1, VInt i2) -> k (VBool (i1 < i2))
  | _ -> runtime_error "binop"

(* thunk -> pat -> env *)
let rec destruct_thunk_to_env t = function
  | EVar x ->
      if x = ignore_id then Env.empty
      else Env.singleton x t
  | EInt _
  | EBool _
  | ENil -> Env.empty
  | EBinOp (Cons, hd, tl) ->
      begin match !t with
        | Value (VCons (thd, ttl)) ->
            let env_hd = destruct_thunk_to_env thd hd in
            let env_tl = destruct_thunk_to_env ttl tl in
            Env.union (fun k a b -> Some a) env_hd env_tl
        | _ -> runtime_error ("not a list")
      end
  | ETuple es ->
      begin match !t with
        | Value (VTuple ts) ->
          if List.length es != List.length ts then
            runtime_error ("tuple size not match")
          else
            List.combine ts es
            |> List.map (fun (t, e) -> destruct_thunk_to_env t e)
            |> List.fold_left (Env.union (fun k a b -> Some b)) Env.empty
        | _ -> runtime_error ("not a tuple")
      end
  | p -> runtime_error ("invalid pattern: " ^ string_of_exp p)

let rec force t k =
  let k v = t := Value v; k v in
  let p = !t in match p with
  | Promise (e, env) ->
      begin match e with
      | EVar x ->
          begin match Env.find_opt x env with
            | Some t' -> force t' (fun v ->
                t' := Value v;
                k v
              )
            | None -> runtime_error ("Variable " ^ x ^ " is not bounded")
          end
      | EInt i -> k (VInt i)
      | EBool b -> k (VBool b)
      | ENil -> k VNil
      | EUndefined -> VUndefined
      | EBinOp (op, e1, e2) ->
          eval env e1 (fun t1 ->
            eval env e2 (fun t2 ->
              match op with
                | Cons -> k (VCons (t1, t2))
                | _ ->
                    force t1 (fun v1 ->
                      t1 := Value v1;
                      force t2 (fun v2 ->
                        t2 := Value v2;
                        eval_binop op v1 v2 (fun v -> k v)
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
                      force t (fun v -> k v)
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
      | EMatchWith (e1, guards) ->
          eval env e1 (fun t1 ->
            force t1 (fun v1 ->
              t1 := Value v1;
              match List.find_opt (fun (p, _) -> can_eval_guard p t1) guards with
                | Some (p, e) ->
                    let env' =
                      destruct_thunk_to_env t1 p
                      |> Env.union (fun k a b -> Some b) env
                    in
                    eval env' e (fun t ->
                      force t (fun v ->
                        t := Value v;
                        k v
                      )
                    )
                | None -> runtime_error "match failure"
            )
          )
      | ETuple es ->
          let rec fold_eval f a xs k = match xs with
            | [] -> k a
            | eh :: et ->
                eval env eh (fun th ->
                  fold_eval f (f a th) et k
                )
          in
          fold_eval (fun x y -> y :: x) [] es (fun ts ->
            k (VTuple (List.rev ts))
          )
    end
  | Value v -> k v
  | Exception -> VUndefined

and can_eval_guard p t = match p with
  | EVar x -> true
  | EInt i ->
      let v = force t (fun v -> t := Value v; v) in
      begin match v with
        | VInt i' -> i = i'
        | _ -> false
      end
  | EBool b ->
      let v = force t (fun v -> t := Value v; v) in
      begin match v with
        | VBool b' -> b = b'
        | _ -> false
      end
  | ENil ->
      let v = force t (fun v -> t := Value v; v) in
      v = VNil
  | EBinOp (Cons, hd, tl) ->
      let v = force t (fun v -> t := Value v; v) in
      begin match v with
        | VCons (thd, ttl) ->
            can_eval_guard hd thd && can_eval_guard tl ttl
        | _ -> false
      end
  | ETuple es ->
      let v = force t (fun v -> t := Value v; v) in
      begin match v with
        | VTuple vs ->
            if List.length es != List.length vs then false
            else
              List.combine es vs
              |> List.for_all (fun (e, v) -> can_eval_guard e v)
        | _ -> false
      end
  | p -> runtime_error ("invalid pattern: " ^ string_of_exp p)

and eval env exp k = match exp with
  | EVar x ->
      begin match Env.find_opt x env with
        | Some t -> k t
        | None -> runtime_error ("Variable " ^ x ^ " is not bounded")
      end
  | EInt i -> k (ref (Value (VInt i)))
  | EBool b -> k (ref (Value (VBool b)))
  | ENil -> k (ref (Value VNil))
  | EUndefined -> k (ref Exception)
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
  | EMatchWith (e1, guards) ->
      k (ref (Promise (EMatchWith (e1, guards), env)))
  | ETuple es ->
      k (ref (Promise (ETuple es, env)))

let rec string_of_value = function
  | VInt i -> string_of_int i
  | VBool b -> string_of_bool b
  | VProc _ -> "<fun>"
  | VNil -> "[]"
  | VCons (t1, t2) ->
      let rec string_of_list_body = function
        | VNil -> ""
        | VCons (t1, t2) ->
            let v1 = force t1 (fun v -> t1 := Value v; v) in
            let v2 = force t2 (fun v -> t2 := Value v; v) in
            let s1 = string_of_value v1 in
            begin match v2 with
              | VNil -> s1
              | VCons _ ->
                  let s2 = string_of_list_body v2 in
                  s1 ^ "; " ^ s2
              | v -> failwith ("invalid list: " ^ string_of_value v)
            end
        | v -> failwith ("invalid list: " ^ string_of_value v)
      in
      Printf.sprintf "[%s]" (string_of_list_body (VCons (t1, t2)))
  | VTuple ts ->
      let vs' =
        ts
        |> List.map (fun t -> force t (fun v -> t := Value v; v))
        |> List.map string_of_value
        |> String.concat ", "
      in
      Printf.sprintf "(%s)" vs'
  | VUndefined -> "undefined"

let start exp =
  eval Env.empty exp (fun t ->
    force t (fun v -> v)
  )
