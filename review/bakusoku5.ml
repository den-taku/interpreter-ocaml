let is_positive n = 
  if n > 0 then print_string "n is positive\n"
  else print_string "n is not positive\n"
;;

let is_positive' n =
  if n > 0 then 
    begin
      print_int n;
      print_string " is positive"
    end
  else 
    begin
      print_int n; 
      print_string " isn't positive"
    end
;;
