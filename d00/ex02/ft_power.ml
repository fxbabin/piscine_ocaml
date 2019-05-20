let rec ft_power num pow: int =
  if pow == 0 then 1  
  else num * (ft_power (num) (pow - 1))

let main() =
  print_int(ft_power 5 3);
  print_endline("");
  print_int(ft_power 2 0);
  print_endline("");
  print_int(ft_power 2 1);
  print_endline("");
  print_int(ft_power 2 3);
  print_endline("");
;;

let () = main ()