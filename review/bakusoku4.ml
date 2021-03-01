type mutable_point = {mutable x: int; mutable y: int;};;

let m_origin = {x = 0; y = 0;};;

m_origin.x <- 2;;
m_origin;;

print_int;;
print_int (5+15);;

type mutable_const = {mutable f: float};;

let pi = {f = 3.14};;

let area r = pi.f *. r *. r;;

area 5.0;;

pi.f <- 3.0;;

area 5.0;;

let x = ref 1;;
let y = ref 3.14;;

x := 2;;
!x;;
x := !x + 1;;
!x;;
