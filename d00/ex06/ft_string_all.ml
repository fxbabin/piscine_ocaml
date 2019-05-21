let is_digit c =
  if c >= '0' && c <= '9' then true
  else false 

let ft_string_all f str =
  let rec loop idx str_len =
    if idx == str_len then true
    else if f (String.get str idx) == false then false 
    else loop (idx + 1) str_len
  in
  loop 0 (String.length str)

let print_bool boo = 
  if boo == true then print_string("true\n")
  else print_string("false\n")

let main () =
  print_bool(ft_string_all is_digit "0123456789");
  print_bool(ft_string_all is_digit "0123456789s");
  print_bool(ft_string_all is_digit "");
  print_bool(ft_string_all is_digit "swdded")

let () = main ()