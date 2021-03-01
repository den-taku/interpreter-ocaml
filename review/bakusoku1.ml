1 + 2 * 3;;

5 = 2 || 9 > -5

let abs n = 
  if n < 0 then -n
  else n
;;

let rec fact n =
  if n = 0 then 1 else n * fact (n-1)
;;

let rec squaresum n =
  let square m = m * m in
  if n = 1 then square 1
  else square n + squaresum (n-1)
;;

