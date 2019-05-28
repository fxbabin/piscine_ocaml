let sum (f1: float) (f2: float) =
  f1 +. f2

let () =
  print_float(sum 0.0 0.0);
  print_endline("");
  print_float(sum 5.0 3.0);
  print_endline("");
  print_float(sum 5.0 (-10.0));
  print_endline("");
  print_float(sum 8.0 99.0);
  print_endline("")