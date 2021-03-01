type tree =
   Lf
  |Br of {
      mutable left: tree;
      mutable value: int;
      mutable right: tree;
    }
;;

let t1 = Br {left = Lf; value = 10; right = Lf};;
let t2 = Br {left = Lf; value = 25; right = Lf};;
let t3 = Br {left = t1; value = 15; right = t2};;
let t4 = Br {left = Lf; value = 60; right = Lf};;
let t5 = Br {left = Lf; value = 48; right = t4};;
let t6 = Br {left = t3; value = 30; right = t5};;

let rec find t n =
  match t with
   Lf -> false
  |Br {left=l;value=v;right=r} ->
    if n = v then true
    else if n < v then find l n
    else (* n > v *)find r n
;;

let rec insert t n =
  match t with 
   Lf -> Br {left=Lf;value=n;right=Lf}
  |Br br ->
    if n = br.value then t
    else if n < br.value then (br.left <- insert br.left n; t)
    else (* m > br.value *) (br.right <- insert br.right n; t)
;;

let rec min t =
  match t with
   Lf -> min_int
  |Br {left=Lf;value=v} -> v
  |Br {left=l} -> min l
;;

let rec delete t n =
  match t with
   Lf -> t
  |Br br ->
    if n = br.value then 
      match br.left,br.right with
       Lf, Lf -> Lf
      |Br _, Lf -> br.left
      |Lf, Br _ -> br.right
      |Br _, Br _ -> 
        let m = min br.right in
        br.value <- m;
        br.right <- delete br.right m;
        t
    else if n < br.value then (br.left <- delete br.left n; t)
    else (* n > br.value *) (br.right <- delete br.right n; t)
;;
