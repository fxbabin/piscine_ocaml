let ft_rot_n num str =
  let ft_rot_char c =
    if (c >= 'a' && c <= 'z') then
      char_of_int(int_of_char('a') + ((int_of_char(c) - int_of_char('a') + num) mod 26))
    else if (c >= 'A' && c <= 'Z') then
      char_of_int(int_of_char('A') + ((int_of_char(c) - int_of_char('A') + num) mod 26))
    else c
  in
  String.map ft_rot_char str;
;;

let main () =
  print_string(ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz");
  print_char('\n');
  print_string(ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz");
  print_char('\n');
  print_string(ft_rot_n 42 "0123456789");
  print_char('\n');
  print_string(ft_rot_n 2 "OI2EAS67B9");
  print_char('\n');
  print_string(ft_rot_n 0 "Damned !");
  print_char('\n');
  print_string(ft_rot_n 42 "");
  print_char('\n');
  print_string(ft_rot_n 1 "NBzlk qnbjr !");
  print_char('\n');
;;

let () = main ()