let ft_test_sign num: unit =
    if num >= 0
    then print_endline("positive")
    else print_endline("negative")
;;

let main() =
  ft_test_sign(-42);
  ft_test_sign(-1);
  ft_test_sign(-0);
  ft_test_sign(0);
  ft_test_sign(1);
  ft_test_sign(42);
;;

let () = main ()