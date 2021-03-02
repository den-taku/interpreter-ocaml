type 'elm tree =
   Lf
  |Br of {
      left: 'elm tree;
      value: 'elm;
      right: 'elm tree;
    }
;;

(* Constructing a sample tree holding integers *)
let t1 = Br {left = Lf; value = 10; right = Lf}
let t2 = Br {left = Lf; value = 25; right = Lf}
let t3 = Br {left = t1; value = 15; right = t2}
let t4 = Br {left = Lf; value = 60; right = Lf}
let t5 = Br {left = Lf; value = 48; right = t4}
let t6 = Br {left = t3; value = 30; right = t5}

(* Now let's construct a tree holding strings *)
let t11 = Br {left = Lf; value = "I"; right = Lf}
let t12 = Br {left = Lf; value = "love"; right = Lf}
let t13 = Br {left = t11; value = "OCaml"; right = t12}
let t14 = Br {left = Lf; value = "How"; right = Lf}
let t15 = Br {left = Lf; value = "about"; right = t14}
let t16 = Br {left = t13; value = "you?"; right = t15}

let rec size t = 
  match t with
   Lf -> 0
  |Br {left=l;right=r} -> size l + size r + 1
;;

let max n m =
  if n < m then m else n
;;

let rec depth t =
  match t with
   Lf -> 0
  |Br {left=l;right=r;} -> max (depth l) (depth r) + 1
;;

let rec reflect t =
  match t with
   Lf -> Lf
  |Br {left=l;value=v;right=r} ->
    Br {left = reflect r;
        value = v;
        right = reflect l;
    }
;;

let rec add t e =
  match t with
   Lf -> Br {left=Lf;value=e;right=Lf}
  |Br {left=l;value=v;right=r} ->
    Br {left = add l e;
        value = v;
        right = r}
;;

let rec string_of_int_tree t =
  match t with
   Lf -> "leaf"
  |Br {left=l;value=v;right=r} ->
    "branch(" ^ string_of_int_tree l ^ "," 
    ^ string_of_int v ^ ","
    ^ string_of_int_tree r ^ ")"
;;

string_of_int_tree t6;;

let rec find cmp t n =
  match t with
   Lf -> false
  |Br {left=l;value=v;right=r} ->
    if cmp n v = 0 then true
    else if cmp n v < 0 then find cmp l n
    else (* n > v *) find cmp r n
;;

let rec min t = 
  match t with
   Lf -> invalid_arg "Input can't be a leaf!"
  |Br {left=Lf;value=v;} -> v
  |Br {left=l} -> min l
;;
