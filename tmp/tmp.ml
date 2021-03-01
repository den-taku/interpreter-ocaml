let rec fact n = if n = 0 then 1 else n * fact (n - 1) ;;

let rec f x = 
  if x = 0 then 1 
  else f (x - 1)
;;

5 < 8
;;

let n = 5 * 2 in n + n
;;

let x = 4 * 4 in
let n = 5 * 2 in
    x * n
;;
