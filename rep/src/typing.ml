open Syntax
open MySet

exception Error of string

let err s = raise (Error s)

type subst = (tyvar * ty) list

(* Type environment *)
type tyenv = ty Environment.t

let eqs_of_subst s = 
    let trans (n, ty) = (TyVar n, ty) in
    List.map trans s

let rec unify = function
     [] -> []
    |(ty1, ty2)::rest -> 
        if ty1 = ty2 then unify rest 
        else (match ty1, ty2 with
             TyFun (e11, e12), TyFun(e21, e22) -> unify ((e11,e21)::(e12,e22)::rest)
            |TyVar n, ty -> if member (TyVar n) (freevar_ty ty) then err "Type Error" 
                           else (n, ty) :: unify rest
            |t, TyVar n -> if member (TyVar n) (freevar_ty t) then err "Type Error"
                           else (n, t) :: unify rest
            |_, _ -> err "Type Error"
        ) 

let rec subst_type l t = 
  match l, t with 
   [], _ -> t
  |_, TyInt -> TyInt
  |_, TyBool -> TyBool
  |(id, ty)::rest, TyVar n -> if n = id then subst_type rest ty else subst_type rest (TyVar n)
  |(id, ty)::rest, TyFun (e1, e2) -> TyFun (subst_type ((id,ty)::rest) e1, subst_type ((id,ty)::rest) e2)
  |(id, ty)::rest, TyList t -> TyList (subst_type ((id,ty)::rest) t)

(*
let ty_prim op ty1 ty2 = match op with
   Plus -> (match ty1, ty2 with
                TyInt, TyInt -> TyInt
               | _ -> err ("Argument must be of interger: +"))
  |Mult -> (match ty1, ty2 with
                TyInt, TyInt -> TyInt
               | _ -> err ("Argument must be of integer: *"))
  |Lt -> (match ty1, ty2 with
             TyInt, TyInt -> TyBool
            | _ -> err ("Argument must be of integer: <"))
  |And -> (match ty1, ty2 with
             TyBool, TyBool -> TyBool
            | _ -> err ("Argument must be of boolean: &&"))
  |Or -> (match ty1, ty2 with
             TyBool, TyBool -> TyBool
            | _ -> err ("Argument must be of boolean: ||"))
*)

let ty_prim op ty1 ty2 = match op with
     Plus -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
    |Mult -> ([(ty1, TyInt); (ty2, TyInt)], TyInt)
    |Lt -> ([(ty1, TyInt); (ty2, TyInt)], TyBool)
    |And -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)
    |Or -> ([(ty1, TyBool); (ty2, TyBool)], TyBool)


let rec ty_exp tyenv = function
   Var x ->
(try ([], Environment.lookup x tyenv) with
        Environment.Not_bound -> err ("variable not bound: " ^ x))
  |ILit _ -> ([], TyInt)
  |BLit _ -> ([], TyBool)
  |BinOp (op, exp1, exp2) ->
    let (s1, ty1) = ty_exp tyenv exp1 in
    let (s2, ty2) = ty_exp tyenv exp2 in
    let (eqs3, ty) = ty_prim op ty1 ty2 in
    let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in
    let s3 = unify eqs in (s3, subst_type s3 ty)
  |IfExp (exp1, exp2, exp3) ->
    let (s1, ty1) = ty_exp tyenv exp1 in 
    let (s2, ty2) = ty_exp tyenv exp2 in 
    let (s3, ty3) = ty_exp tyenv exp3 in 
    let new_ty = (TyVar (fresh_tyvar ())) in
    let (eqs4, ty) = ([(ty1, TyBool); (ty2, new_ty); (ty3, new_ty)], new_ty) in
    let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ (eqs_of_subst s3) @ eqs4 in
    let s4 = unify eqs in (s4, subst_type s4 ty)
  |LetExp (id, exp1, exp2) -> (*TODO: here*)
    let (s1, ty1) = ty_exp tyenv exp1 in
    let (s2, ty2) = ty_exp (Environment.extend id ty1 tyenv) exp2 in
    let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) in
    let s3 = unify eqs in (s3, subst_type s3 ty2)
  |FunExp (id, exp) ->
    let domty = TyVar (fresh_tyvar ()) in
    let s, ranty =
        ty_exp (Environment.extend id domty tyenv) exp in
        (s, TyFun (subst_type s domty, ranty))
  |AppExp (exp1, exp2) ->
    let (s1, ty1) = ty_exp tyenv exp1 in
    let (s2, ty2) = ty_exp tyenv exp2 in
    let domty = TyVar (fresh_tyvar ()) in
    let eqs3 = [(ty1, TyFun (ty2, domty));] in
    let eqs = (eqs_of_subst s1) @ (eqs_of_subst s2) @ eqs3 in
    let s3 = unify eqs in (s3, subst_type s3 domty)
  |_ -> err ("Not Implemented!: ty_exp")

let ty_decl tyenv = function
   Exp e -> ty_exp tyenv e
  (* |Decl (id, e) -> 
    let v = ty_exp tyenv e in (id, Environment.extend id v tyenv, v) *)
  |_ -> err ("Not Implemented!: ty_decl")

let subst_eqs s eqs =
    map (subst_type s) eqs
































































































