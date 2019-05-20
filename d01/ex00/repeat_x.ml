let repeat_x n =
  if n < 0 then "Error"
  else begin
    let rec repeat_aux idx acc =
      if idx == n then acc
      else repeat_aux (idx + 1) (acc ^ "x")
    in
    repeat_aux 0 "";
  end
;;

let main () =
  print_string(repeat_x (-1));
  print_endline("");
  print_string(repeat_x 0);
  print_endline("");
  print_string(repeat_x 1);
  print_endline("");
  print_string(repeat_x 2);
  print_endline("");
  print_string(repeat_x 5);
  print_endline("");
;;

let () = main ()