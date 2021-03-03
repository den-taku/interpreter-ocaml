type one = I and two = II

let rec string_of_one = function
  I -> "One"
;;

let f (a, b) = a * b ;;

let twice f x = f (f x);;

let four_times = twice @@ twice ;;

let i = four_times (fun x -> x * x) 9;;
