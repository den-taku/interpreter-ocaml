open Syntax
open MySet

exception Error of string

let err s = raise (Error s)

type subst = (tyvar * ty) list

(* Type environment *)
type tyenv = ty Environment.t

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

let rec ty_exp tyenv = function
   Var x ->
    (try Environment.lookup x tyenv with
        Environment.Not_bound -> err ("variable not bound: " ^ x))
  |ILit _ -> TyInt
  |BLit _ -> TyBool
  |BinOp (op, exp1, exp2) ->
    let tyarg1 = ty_exp tyenv exp1 in
    let tyarg2 = ty_exp tyenv exp2 in
        ty_prim op tyarg1 tyarg2
  |IfExp (exp1, exp2, exp3) ->
    let tyarg1 = ty_exp tyenv exp1 in 
    let tyarg2 = ty_exp tyenv exp2 in 
    let tyarg3 = ty_exp tyenv exp3 in 
        (match tyarg1 with
            TyBool -> if tyarg2 = tyarg3 then tyarg2
                else err ("This expression has type " ^ (string_of_type tyarg3) ^ " but an expression was expected of type " ^ string_of_type tyarg2)
      | _ -> err ("This expression has type " ^ (string_of_type tyarg1) ^ " but an expression was expected of type bool"))
  |LetExp (id, exp1, exp2) -> 
    let tyarg1 = ty_exp tyenv exp1 in
    ty_exp (Environment.extend id tyarg1 tyenv) exp2
  |_ -> err ("Not Implemented!")

let ty_decl tyenv = function
   Exp e -> ty_exp tyenv e
  |_ -> err ("Not Implemented!")

let rec subst_type l t = 
  match l, t with 
   [], _ -> t
  |_, TyInt -> TyInt
  |_, TyBool -> TyBool
  |(id, ty)::rest, TyVar n -> if n = id then subst_type rest ty else subst_type rest (TyVar n)
  |(id, ty)::rest, TyFun (e1, e2) -> TyFun (subst_type ((id,ty)::rest) e1, subst_type ((id,ty)::rest) e2)
  |(id, ty)::rest, TyList t -> TyList (subst_type ((id,ty)::rest) t)

let rec unify = function
     [] -> []
    |(ty1, ty2)::rest -> 
        if ty1 = ty2 then unify rest 
        else (match ty1, ty2 with
             TyFun (e11, e12), TyFun(e21, e22) -> unify ((e11,e21)::(e12,e22)::rest)
            |TyVar n, ty -> if member (TyVar n) (freevar_ty ty) then err "Type Error" 
                           else (TyVar n, ty) :: unify rest
            |t, TyVar n -> if member (TyVar n) (freevar_ty t) then err "Type Error"
                           else (t, TyVar n) :: unify rest
            |_, _ -> err "Type Error"
        ) 



































































































