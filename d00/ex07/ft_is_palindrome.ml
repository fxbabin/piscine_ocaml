let ft_is_palindrome str =
  let rec loop idx str_len =
    if idx > str_len then true
    else if (String.get str idx) <> (String.get str str_len) then false
    else loop (idx + 1) (str_len - 1)
  in
  loop 0 ((String.length str) - 1)

let print_bool boo = 
  if boo == true then print_string("true\n")
  else print_string("false\n")

let main () =
  print_bool(ft_is_palindrome "odo");
  print_bool(ft_is_palindrome "odo");
  print_bool(ft_is_palindrome "");
  print_bool(ft_is_palindrome "0");
  print_bool(ft_is_palindrome "00");
  print_bool(ft_is_palindrome "01");
  print_bool(ft_is_palindrome "radar");
  print_bool(ft_is_palindrome "madam");
  print_bool(ft_is_palindrome "car");
  print_bool(ft_is_palindrome "")

let () = main ()