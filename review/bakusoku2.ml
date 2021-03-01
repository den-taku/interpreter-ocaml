type point = { x: int; y: int; };;

let origin = { x = 0; y = 0; };;

origin.x;;

origin.y;;

let average (x,y) = (x + y) / 2 ;;

let middle(p1, p2) =
  {x = average(p1.x, p2.x); y = average(p1.y, p2.y) };;

middle(origin, {x=4; y=5});;

let {x=ox;y=oy} = origin;;

let get_x {x} = x;;

let area ({x=p1x;y=p1y}, {x=p2x;y=p2y}) =
  let dx = p1x - p2x in
  let dy = p1y - p2y in
  sqrt (float_of_int (dx * dx + dy * dy))
;;

type rational = {
  num: int; (* numerator *)
  den: int; (* denominator *)
};;

let rec gcd (x,y) = 
  if x < y then gcd(y-x,x) 
  else if x > y then gcd(x-y,y)
  else x
;;

let sum ({num=n1;den=d1},{num=n2;den=d2}) = 
  let new'num = n1 * d2 + n2 * d1 in
  let new'den = d1 * d2 in
  let gcd' = gcd (new'num, new'den) in
  {num = new'num / gcd'; den = new'den / gcd'}
;;

let p = {origin with y = 3};;
