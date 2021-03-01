type furikake = Shake | Katsuo | Nori;;

Shake;;

let isVeggie f = 
  match f with 
   Shake -> false
  |Katsuo -> false 
  |Nori -> true
;;
 
let n = 90;;

match n with
  2 | 3 | 5 | 7 | 11 | 13 | 17 | 19 -> true
  | _ -> false
;;
 
type miso = Aka | Shiro | Awase ;;
type gu = Wakame | Tofu | Radish ;;
type dish = PorkCutlet | Soup of {m: miso; g: gu} | Rice of furikake;;

PorkCutlet;;
Soup {m = Aka; g = Tofu};;
Rice Shake;;

let isSolid d =
  match d with
   PorkCutlet -> true
  |Soup m_and_g -> false
  |Rice f -> true
;;

(*
let price_of_dich d =
  match d with
   PorkCutlet -> 350
  |Soup m_and_g -> 90
  |Rice f -> (match f with Shake -> 90 | Katsuo -> 90 | Nori -> 80)
;;
*)

let price_of_dish d =
  match d with
   PorkCutlet -> 350
  |Soup m_and_g -> 90
  |Rice (Shake|Katsuo) -> 90
  |Rice Nori -> 80
;;

let isVeggieDish d =
  match d with
   PorkCutlet | Rice (Shake|Katsuo) -> false
  |_ -> true
;;
