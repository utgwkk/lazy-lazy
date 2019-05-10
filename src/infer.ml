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

let rec infer tyenv exp k = match exp with
  | EVar x ->
      begin match Env.find_opt x tyenv with
        | Some (TScheme (vars, t)) ->
            let s =
              vars
              |> List.map (fun id -> (id, TVar (fresh_tyvar ())))
            in
            k ([], subst_ty s t)
        | None -> infer_failed ("Variable " ^ x ^ " is not bounded")
      end
  | EInt _ -> k ([], TInt)
  | EBool _ -> k ([], TBool)
  | ENil ->
      let tv = fresh_tyvar () in
      k ([], TList (TVar tv))
  | EUndefined ->
      let tv = fresh_tyvar () in
      k ([], TVar tv)
  | EBinOp (op, e1, e2) ->
      infer tyenv e1 (fun (s1, t1) ->
        infer tyenv e2 (fun (s2, t2) ->
          let (eqs_op, ty) = infer_op op t1 t2 in
          let eqs = eqs_of_subst s1 @ eqs_of_subst s2 @ eqs_op in
          let s3 = unify eqs in
          k (s3, subst_ty s3 ty)
        )
      )
  | EIfThenElse (e1, e2, e3) ->
      infer tyenv e1 (fun (s1, t1) ->
        infer tyenv e2 (fun (s2, t2) ->
          infer tyenv e3 (fun (s3, t3) ->
            let eqs =
              [(t1, TBool); (t2, t3)]
              @ eqs_of_subst s1 @ eqs_of_subst s2 @ eqs_of_subst s3
            in
            let s4 = unify eqs in
            k (s4, subst_ty s4 t2)
          )
        )
      )
  | ELet (x, e1, e2) ->
      infer tyenv e1 (fun (s1, t1) ->
        let tysc1 = closure t1 tyenv s1 in
        let tyenv' = Env.add x tysc1 tyenv in
        infer tyenv' e2 (fun (s2, t2) ->
          let eqs = eqs_of_subst s1 @ eqs_of_subst s2 in
          let s3 = unify eqs in
          k (s3, subst_ty s3 t2)
        )
      )
  | EAbs (x, e) ->
      let tx = TVar (fresh_tyvar ()) in
      let tyenv' = Env.add x (tysc_of_ty tx) tyenv in
      infer tyenv' e (fun (s1, t1) ->
        k (s1, TFun (subst_ty s1 tx, t1))
      )
  | EApp (e1, e2) ->
      infer tyenv e1 (fun (s1, t1) ->
        infer tyenv e2 (fun (s2, t2) ->
          let tr = TVar (fresh_tyvar ()) in
          let eqs =
            [(t1, TFun (t2, tr))] @ eqs_of_subst s1 @ eqs_of_subst s2
          in
          let s3 = unify eqs in
          k (s3, subst_ty s3 tr)
        )
      )
  | ELetRec (x, e1, e2) ->
      let tx = TVar (fresh_tyvar ()) in
      let tyenv' = Env.add x (tysc_of_ty tx) tyenv in
      infer tyenv' e1 (fun (s1, t1) ->
        let eqs = [(tx, t1)] @ eqs_of_subst s1 in
        let s = unify eqs in
        let tysc1 = closure (subst_ty s t1) tyenv s1 in
        let tyenv'' = Env.add x tysc1 tyenv in
        infer tyenv'' e2 (fun (s2, t2) ->
          k (s2, t2)
        )
      )
  | EMatchWith (e1, enil, xcar, xcdr, econs) ->
      infer tyenv e1 (fun (s1, t1) ->
        infer tyenv enil (fun (snil, tnil) ->
          let th = TVar (fresh_tyvar ()) in
          let tyenv' =
            tyenv
            |> Env.add xcar (tysc_of_ty th)
            |> Env.add xcdr (tysc_of_ty (TList th))
          in
          infer tyenv' econs (fun (scons, tcons) ->
            let eqs =
              [(t1, TList th); (tnil, tcons)]
              @ eqs_of_subst s1 @ eqs_of_subst snil @ eqs_of_subst scons
            in
            let s = unify eqs in
            k (s, subst_ty s tnil)
          )
        )
      )

let start exp =
  infer Env.empty exp snd
