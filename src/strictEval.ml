open Syntax

type value =
  | VInt of int
  | VBool of bool
  | VProc of id * exp * value Env.t ref
  | VNil
  | VCons of value * value
  | VTuple of value list
  | VUndefined

let rec string_of_value = function
  | VInt i -> string_of_int i
  | VBool b -> string_of_bool b
  | VProc _ -> "<fun>"
  | VNil -> "[]"
  | VCons (v1, v2) ->
      let rec string_of_list_body = function
        | VNil -> ""
        | VCons (v1, v2) ->
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
      Printf.sprintf "[%s]" (string_of_list_body (VCons (v1, v2)))
  | VTuple vs ->
      vs
      |> List.map string_of_value
      |> String.concat ", "
      |> Printf.sprintf "(%s)"
  | VUndefined -> "undefined"

exception Error of string
let runtime_error s = raise (Error s)

let eval_binop op v1 v2 k = match (op, v1, v2) with
  | (Plus, VInt i1, VInt i2) -> k (VInt (i1 + i2))
  | (Mult, VInt i1, VInt i2) -> k (VInt (i1 * i2))
  | (Lt, VInt i1, VInt i2) -> k (VBool (i1 < i2))
  | (Cons, v1, v2) -> k (VCons (v1, v2))
  | _ -> runtime_error "binop"

let newer _ _ b = Some b

(* value -> pat -> env *)
let rec destruct_value_to_env v = function
  | EVar x ->
      if x = ignore_id then Env.empty
      else Env.singleton x v
  | EInt _
  | EBool _
  | ENil -> Env.empty
  | EBinOp (Cons, hd, tl) ->
      begin match v with
        | VCons (vhd, vtl) ->
            let env_hd = destruct_value_to_env vhd hd in
            let env_tl = destruct_value_to_env vtl tl in
            Env.union newer env_hd env_tl
        | _ -> runtime_error ("not a list: " ^ string_of_value v)
      end
  | ETuple es ->
      begin match v with
        | VTuple vs ->
          if List.length es != List.length vs then
            runtime_error ("tuple size not match")
          else
            List.combine vs es
            |> List.map (fun (v, e) -> destruct_value_to_env v e)
            |> List.fold_left (Env.union newer) Env.empty
        | _ -> runtime_error ("not a tuple: " ^ string_of_value v)
      end
  | p -> runtime_error ("invalid pattern: " ^ string_of_exp p)

let rec can_eval_guard p v = match p with
  | EVar x -> true
  | EInt i ->
      begin match v with
        | VInt i' -> i = i'
        | _ -> false
      end
  | EBool b ->
      begin match v with
        | VBool b' -> b = b'
        | _ -> false
      end
  | ENil ->
      begin match v with
        | VNil -> true
        | _ -> false
      end
  | EBinOp (Cons, hd, tl) ->
      begin match v with
        | VCons (vhd, vtl) ->
            can_eval_guard hd vhd && can_eval_guard tl vtl
        | _ -> false
      end
  | ETuple es ->
      begin match v with
        | VTuple vs ->
            if List.length es != List.length vs then false
            else
              List.combine es vs
              |> List.for_all (fun (e, v) -> can_eval_guard e v)
        | _ -> false
      end
  | p -> runtime_error ("invalid pattern: " ^ string_of_exp p)

let rec eval env exp k = match exp with
  | EVar x ->
      begin match Env.find_opt x env with
        | Some v -> k v
        | None -> runtime_error ("Variable " ^ x ^ " is not bounded")
      end
  | EInt i -> k (VInt i)
  | EBool b -> k (VBool b)
  | ENil -> k VNil
  | EUndefined -> VUndefined
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
  | EMatchWith (e1, guards) ->
      eval env e1 (fun v1 ->
        match List.find_opt (fun (p, _) -> can_eval_guard p v1) guards with
          | Some (p, e) ->
              let env' =
                destruct_value_to_env v1 p
                |> Env.union newer env
              in eval env' e (fun v2 -> k v2)
          | None -> runtime_error "match failure"
      )
  | ETuple es ->
      let rec fold_eval f a xs k = match xs with
        | [] -> k a
        | eh :: et ->
            eval env eh (fun vh ->
              fold_eval f (f a vh) et k
            )
      in
      fold_eval (fun x y -> y :: x) [] es (fun vs ->
        k (VTuple (List.rev vs))
      )

let start exp =
  eval Env.empty exp (fun x -> x)
