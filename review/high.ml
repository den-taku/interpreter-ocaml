let rec sigma f n =
  if n < 1 then f 0
  else f n + sigma f (n-1)
;;

let square n = n * n;;
let cube n = n * n * n;;

let a = sigma square 20;;
let b = sigma cube 20;;

let c = sigma (fun n -> n * n) 20 ;;
let d = sigma (fun n -> n * n * n) 20;;

type gender = Male | Female;;

let curried_greeting gen = fun name ->
  match gen with
   Male -> "Hello, Mr." ^ name
  |Female -> "Hello, Ms." ^ name
;;

let g1 = (curried_greeting Male) "Poirot";;
let g2 = curried_greeting Female "Marple";;

type tree =
   Lf
  |Br of {
      left: tree;
      value: int;
      right: tree;
    }
;;

let t1 = Br {left = Lf; value = 10; right = Lf};;
let t2 = Br {left = Lf; value = 25; right = Lf};;
let t3 = Br {left = t1; value = 15; right = t2};;
let t4 = Br {left = Lf; value = 60; right = Lf};;
let t5 = Br {left = Lf; value = 48; right = t4};;
let t6 = Br {left = t3; value = 30; right = t5};;

let rec treemap f t =
  match t with
   Lf -> Lf
  |Br {left=l;value=v;right=r} ->
    Br {left = treemap f l;
        value = f v;
        right = treemap f r;}
;;

let t7 = treemap (fun n -> n * 2) t6;;

let rec treefold e f t =
  match t with
   Lf -> e
  |Br {left=l;value=v;right=r} ->
    f (treefold e f l) v (treefold e f r)
;;

let s6 = treefold 0 (fun l v r -> l + v + r) t6 ;;
let s7 = treefold 0 (fun l v r -> l + v + r) t7 ;;
