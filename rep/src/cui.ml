open Eval
open Typing
open Syntax

let rec read_eval_print env tyenv =
  print_string "# ";
  flush stdout;
  try
    let decl = Parser.toplevel Lexer.main (Lexing.from_channel stdin) in
    let (newtyenv, ty) = ty_decl tyenv decl in
    let (id, newenv, v) = eval_decl env decl in
    Printf.printf "val %s = " id;
    pp_ty ty;
    print_string " = ";
    pp_val v;
    print_newline();
    read_eval_print newenv newtyenv
  with
    Eval.Error e -> (print_string "Error: ";
                     print_string e;
                     print_newline();
                     read_eval_print env tyenv)
  |Typing.Error e -> (print_string "Error: ";
                      print_string e;
                      print_newline();
                     read_eval_print env tyenv)
                     (*
  | _ -> (print_string "Fatal error";
          print_newline();
          read_eval_print env tyenv)
*)

let initial_env =
  Environment.extend "i" (IntV 1)
    (Environment.extend "ii" (IntV 2)
       (Environment.extend "iii" (IntV 3)
          (Environment.extend "iv" (IntV 4)
             (Environment.extend "v" (IntV 5)
                (Environment.extend "x" (IntV 10) Environment.empty)))))

let initial_tyenv =
  Environment.extend "i" TyInt
    (Environment.extend "ii" TyInt
        (Environment.extend "iii" TyInt
            (Environment.extend "iv" TyInt
                (Environment.extend "v" TyInt
                    (Environment.extend "x" TyInt Environment.empty)))))
