open Char
open MySet


(* ML interpreter / type reconstruction *)
type id = string

type binOp = Plus | Mult | Lt | And | Or

type exp =
    Var of id
  | ILit of int
  | BLit of bool
  | BinOp of binOp * exp * exp
  | IfExp of exp * exp * exp
  | LetExp of id * exp * exp
  | FunExp of id * exp
  | AppExp of exp * exp
  | LetRecExp of id * id * exp * exp

type program =
    Exp of exp
  | Decl of id * exp
  | RecDecl of id * id * exp

type tyvar = int

type ty =
    TyInt
  | TyBool
  | TyVar of tyvar
  | TyFun of ty * ty
  | TyList of ty

let string_of_number n =
  let c = char_of_int (97 + (n mod 26)) in
  escaped c

let rec pp_ty typ =
  match typ with
   TyInt -> print_string "int"
  |TyBool -> print_string "bool"
  |TyVar n -> print_string ("'" ^ string_of_number n)
  |TyFun (ty1, ty2) -> (pp_ty ty1; print_string " -> "; pp_ty ty2) 
  |_ -> print_string "Not Implemented!: pp_ty"

let string_of_type =
  function 
   TyInt -> "int"
  |TyBool -> "bool"
  |_ -> "Not Implemented!: string_of_type"

let fresh_tyvar =
  let counter = ref 0 in
  let body () =
    let v = !counter in
        counter := v + 1; v
  in body

let rec freevar_ty ty = 
  match ty with
   TyInt -> MySet.empty
  |TyBool -> MySet.empty
  |TyVar n -> insert (TyVar n) MySet.empty
  |TyFun (e1, e2) -> union (freevar_ty e1) (freevar_ty e2)
  | _ -> MySet.empty
