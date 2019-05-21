let fibonacci n =
  let rec fibo_aux prev acc n =
    if n < 0 then (-1)
    else if n == 0 then prev
    else if n == 1 then acc
    else fibo_aux acc (prev + acc) (n - 1)
  in fibo_aux 0 1 n

let main () =
  print_int(fibonacci (-1));
  print_endline("");
  print_int(fibonacci 0);
  print_endline("");
  print_int(fibonacci 1);
  print_endline("");
  print_int(fibonacci 2);
  print_endline("");
  print_int(fibonacci 3);
  print_endline("");
  print_int(fibonacci 4);
  print_endline("");
  print_int(fibonacci 5);
  print_endline("");
  print_int(fibonacci 6);
  print_endline("");
  print_int(fibonacci 7);
  print_endline("");
  print_int(fibonacci 100);
  print_endline("")

let () = main ()