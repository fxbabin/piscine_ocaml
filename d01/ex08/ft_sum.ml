let ft_sum (f: int -> float) lower upper = 
  if lower > upper then nan
  else begin
    let rec ft_sum_aux (f: int -> float) lower upper acc = 
      if lower > upper then acc
      else ft_sum_aux f (lower + 1) upper (acc +. (f lower))
    in ft_sum_aux f lower upper 0.0
  end

let main () =
  print_float(ft_sum (fun i -> float_of_int (i * i)) 10 1);
  print_endline("");
  print_float(ft_sum (fun i -> float_of_int (i * i)) 1 10);
  print_endline("")

let () = main ()