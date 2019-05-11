open Syntax

type subst = (tyvar * ty) list

let subst_ty subs t =
  let rec subst_ty (tv, t) = function
    | TInt -> TInt
    | TBool -> TBool
    | TVar tv' ->
        if tv = tv' then t
        else TVar tv'
    | TFun (t1, t2) ->
        TFun (subst_ty (tv, t) t1, subst_ty (tv, t) t2)
    | TList t' -> TList (subst_ty (tv, t) t')
    | TTuple ts ->
        TTuple (List.map (subst_ty (tv, t)) ts)
  in
  List.fold_left (fun t s -> subst_ty s t) t subs

let subst_eqs s eqs = 
  List.map (fun (t1, t2) -> (subst_ty s t1, subst_ty s t2)) eqs

let eqs_of_subst subs =
  List.map (fun (tv, t) -> (TVar tv, t)) subs

let rec occurs tv = function
  | TInt
  | TBool -> false
  | TVar tv' -> tv = tv'
  | TFun (t1, t2) ->
      occurs tv t1 || occurs tv t2
  | TList t -> occurs tv t
  | TTuple ts ->
      List.exists (occurs tv) ts

exception Unify_failed of string
let unify_failed s = raise (Unify_failed s)

let rec unify = function
  | [] -> []
  | (t1, t2) :: tl ->
      if t1 = t2 then unify tl
      else match (t1, t2) with
        | TFun (t11, t12), TFun (t21, t22) ->
            unify ((t11, t21) :: (t12, t22) :: tl)
        | TList t1, TList t2 ->
            unify ((t1, t2) :: tl)
        | TTuple ts1, TTuple ts2 ->
            if List.length ts1 != List.length ts2 then
              unify_failed ("Size of tuple unmatch")
            else
              let eqs' = List.combine ts1 ts2 in
              unify (eqs' @ tl)
        | TVar tv, t ->
            if occurs tv t then
              unify_failed ("Type variable " ^ string_of_int tv ^ " occurs.")
            else
              let tl' = subst_eqs [(tv, t)] tl in
              (tv, t) :: unify tl'
        | t, TVar tv ->
            unify ((TVar tv, t) :: tl)
        | _ -> unify_failed "Type mismatch"

let fresh_tyvar =
  let counter = ref 0 in
  let body () =
    incr counter;
    !counter
  in body

let freevar_tyenv tyenv =
  Env.fold (fun _ tysc set ->
    TV.union (ftv_tysc tysc) set
  ) tyenv TV.empty

let closure ty tyenv subst =
  let fv_tyenv' = freevar_tyenv tyenv in
  let fv_tyenv =
    TV.fold (fun tv set ->
      let ftvs = ftv (subst_ty subst (TVar tv)) in
      TV.union set ftvs
    ) fv_tyenv' TV.empty
  in
  let ids = TV.diff (ftv ty) fv_tyenv in
  TScheme (TV.elements ids, ty)

exception Infer_failed of string
let infer_failed s = raise (Infer_failed s)

let infer_op op t1 t2 = match op with
  | Plus -> ([(t1, TInt); (t2, TInt)], TInt)
  | Mult -> ([(t1, TInt); (t2, TInt)], TInt)
  | Lt -> ([(t1, TInt); (t2, TInt)], TBool)
  | Cons ->
      let tv = fresh_tyvar () in
      let t = TVar tv in
      ([(t1, t); (t2, TList t)], TList t)

let rec infer tyenv = function
  | EVar x ->
      begin match Env.find_opt x tyenv with
        | Some (TScheme (vars, t)) ->
            let s =
              vars
              |> List.map (fun id -> (id, TVar (fresh_tyvar ())))
            in
            ([], subst_ty s t)
        | None -> infer_failed ("Variable " ^ x ^ " is not bounded")
      end
  | EInt _ -> ([], TInt)
  | EBool _ -> ([], TBool)
  | ENil ->
      let tv = fresh_tyvar () in
      ([], TList (TVar tv))
  | EUndefined ->
      let tv = fresh_tyvar () in
      ([], TVar tv)
  | EBinOp (op, e1, e2) ->
      let (s1, t1) = infer tyenv e1 in
      let (s2, t2) = infer tyenv e2 in
      let (eqs_op, ty) = infer_op op t1 t2 in
      let eqs = eqs_of_subst s1 @ eqs_of_subst s2 @ eqs_op in
      let s3 = unify eqs in
      (s3, subst_ty s3 ty)
  | EIfThenElse (e1, e2, e3) ->
      let (s1, t1) = infer tyenv e1 in
      let (s2, t2) = infer tyenv e2 in
      let (s3, t3) = infer tyenv e3 in
      let eqs =
        [(t1, TBool); (t2, t3)]
        @ eqs_of_subst s1 @ eqs_of_subst s2 @ eqs_of_subst s3
      in
      let s4 = unify eqs in
      (s4, subst_ty s4 t2)
  | ELet (x, e1, e2) ->
      let (s1, t1) = infer tyenv e1 in
      let tysc1 = closure t1 tyenv s1 in
      let tyenv' = Env.add x tysc1 tyenv in
      let (s2, t2) = infer tyenv' e2 in
      let eqs = eqs_of_subst s1 @ eqs_of_subst s2 in
      let s3 = unify eqs in
      (s3, subst_ty s3 t2)
  | EAbs (x, e) ->
      let tx = TVar (fresh_tyvar ()) in
      let tyenv' = Env.add x (tysc_of_ty tx) tyenv in
      let (s1, t1) = infer tyenv' e in
      (s1, TFun (subst_ty s1 tx, t1))
  | EApp (e1, e2) ->
      let (s1, t1) = infer tyenv e1 in
      let (s2, t2) = infer tyenv e2 in
      let tr = TVar (fresh_tyvar ()) in
      let eqs =
        [(t1, TFun (t2, tr))] @ eqs_of_subst s1 @ eqs_of_subst s2
      in
      let s3 = unify eqs in
      (s3, subst_ty s3 tr)
  | ELetRec (x, e1, e2) ->
      let tx = TVar (fresh_tyvar ()) in
      let tyenv' = Env.add x (tysc_of_ty tx) tyenv in
      let (s1, t1) = infer tyenv' e1 in
      let eqs = [(tx, t1)] @ eqs_of_subst s1 in
      let s = unify eqs in
      let tysc1 = closure (subst_ty s t1) tyenv s1 in
      let tyenv'' = Env.add x tysc1 tyenv in
      let (s2, t2) = infer tyenv'' e2 in
      (s2, t2)
  | EMatchWith (e1, guards) ->
      (* ty -> exp -> eqs * tyenv *)
      let rec pattern t = function
        | EVar x ->
            if x = ignore_id then ([], Env.empty)
            else ([], Env.singleton x (tysc_of_ty t))
        | EInt _ -> ([(t, TInt)], Env.empty)
        | EBool _ -> ([(t, TBool)], Env.empty)
        | ENil ->
            let tv = TVar (fresh_tyvar ()) in
            ([(t, TList tv)], Env.empty)
        | EBinOp (Cons, hd, tl) ->
            let tv = TVar (fresh_tyvar ()) in
            let (eqs_hd, tyenv_hd) = pattern tv hd in
            let (eqs_tl, tyenv_tl) = pattern (TList tv) tl in
            let tyenv' =
              Env.union (fun k _ _ ->
                infer_failed ("the variable " ^ k ^ " occurs twice in pattern-match expression")
              ) tyenv_hd tyenv_tl
            in
            let eqs = [(t, TList tv)] @ eqs_hd @ eqs_tl in
            (eqs, tyenv')
        | ETuple es ->
            let (eqs, tyenv, ts) =
              List.fold_left (fun (eqs, tyenv, ts) e ->
                let tv = TVar (fresh_tyvar ()) in
                let (eqs', tyenv') = pattern tv e in
                (
                  eqs @ eqs', Env.union (fun k _ _ ->
                    infer_failed ("the variable " ^ k ^ " occurs twice in pattern-match expression")
                  ) tyenv tyenv', ts @ [tv]
                )
              ) ([], Env.empty, []) es
            in
            let eqs' = [(t, TTuple ts)] @ eqs in
            (eqs', tyenv)
        | e -> failwith ("invalid pattern: " ^ string_of_exp e)
      in
      let (s1, t1) = infer tyenv e1 in
      let tv = TVar (fresh_tyvar ()) in
      List.fold_left (fun (s, t) (p, e) ->
        let (eqs, tyenv') = pattern t1 p in
        let tyenv'' =
          Env.union (fun _ _ b -> Some b) tyenv tyenv'
        in
        let (s', t') = infer tyenv'' e in
        let eqs' =
          [(t', t)] @ eqs @ eqs_of_subst s @ eqs_of_subst s'
        in
        let s'' = unify eqs' in
        (s'', subst_ty s'' t')
      ) (s1, tv) guards
  | ETuple es ->
      let (s, ts) =
        List.fold_left (fun (s, ts) e ->
          let (s', t) = infer tyenv e in
          (s @ s', ts @ [t])
        ) ([], []) es
      in
      (s, TTuple ts)

let start exp =
  infer Env.empty exp |> snd
