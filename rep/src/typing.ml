open Syntax

exception Error of string

let err s = raise (Error s)

(* Type environment *)
type tyenv = ty Environment.t

let ty_prim op ty1 ty2 = match op with
   Plus -> (match ty1, ty2 with
                TyInt, TyInt -> TyInt
               | _ -> err ("Argument must be of interger: +"))
  | _ -> err "Not Implemented!"

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
