let repeat_string ?(str="x") n =
  if n < 0 then "Error"
  else begin
    let rec repeat_aux idx acc =
      if idx == n then acc
      else repeat_aux (idx + 1) (acc ^ str)
    in
    repeat_aux 0 "";
  end
;;

let main () =
  print_string(repeat_string (-1));
  print_endline("");
  print_string(repeat_string  0);
  print_endline("");
  print_string(repeat_string  1);
  print_endline("");
  print_string(repeat_string  2);
  print_endline("");
  print_string(repeat_string  5);
  print_endline("");
  print_string(repeat_string ~str:"toto" 3);
  print_endline("");
  print_string(repeat_string ~str:"" 3);
  print_endline("");
  print_string(repeat_string ~str:"xx" 3);
  print_endline("");
;;

let () = main ()