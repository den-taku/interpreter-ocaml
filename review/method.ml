let rec sum n =
  if n = 1 then 1
  else n + sum (n-1)
;;

sum 260000;;
(* sum 270000;; *)

let rec gcd n m =
  if n = m then n
  else if n > m then gcd (n-m) m
  else (* m > n *) gcd (m-n) n
;;

gcd 540001 2;;
(* gcd 540000001 2;; *)

let i = ref 0 in
while !i < 10 do
  print_int (!i+1);
  print_string " times\n";
  i := !i + 1;
done
;;

for i = 0 to 10 
    do
      print_int i;
      print_newline()
    done
;;

for i = 10 downto 1 
    do
      print_int i;
      print_newline()
    done
;;

let gcd' n m =
  let nv = ref n in
  let mv = ref m in
  while !nv <> !mv do
    if !nv > !mv then
      nv := !nv - !mv
    else 
      begin
        let tmp = !nv in
        nv := !mv - tmp;
        mv := tmp
      end
  done;
  !nv
;;

(*
let rec sum' n m =
  if n = 1 then m + 1 else sum' (n-1) (m + n)
;;
*)

let sum' n =
  let rec sum'' n m =
    if n = 1 then m + 1
    else sum'' (n-1) (m+n)
  in sum'' n 0
;;





























