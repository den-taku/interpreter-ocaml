let sum n =
  let rec sum' n m =
    if n = 1 then m + 1 
    else sum' (n-1) (m+n)
  in sum' n 0
;;

sum 10;;

type bt =
   Lf
  |Br of {
      left: bt;
      value: int;
      right: bt
    }
;;

let t1 = Br {left = Lf; value = 10; right = Lf};;
let t2 = Br {left = Lf; value = 25; right = Lf};;
let t3 = Br {left = t1; value = 15; right = t2};;
let t4 = Br {left = Lf; value = 60; right = Lf};;
let t5 = Br {left = Lf; value = 48; right = t4};;
let t6 = Br {left = t3; value = 30; right = t5};;

let rec sumtree t =
  match t with
   Lf -> 0
  |Br {left=l;value=v;right=r} ->
    sumtree l + v + sumtree r
;;

sumtree t6;;

let rec mapTree f t =
  match t with
   Lf -> Lf
  |Br {left=l;value=v;right=r} ->
    Br {
      left = mapTree f l;
      value = f v;
      right = mapTree f r
    }
;;

let t7 = mapTree (fun n -> n * n) t6 ;;




































































































