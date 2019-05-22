let leibniz_pi (delta: float) = 
  if delta < 0.0 then -1
  else begin 
    let pi = (4.0 *. (atan 1.0)) in
    let absolute_float (n: float) =
      if n < 0.0 then -.n
      else n
    in
    let rec sum_leibniz_pi lower acc = 
      if (absolute_float (acc -. pi)) <= delta then lower
      else sum_leibniz_pi (lower + 1) (acc +. (4.0 *. ( (-1.0 ** (float_of_int lower)) /. (2.0 *. (float_of_int lower) +. 1.0))))
    in sum_leibniz_pi 0 0.0
  end

let main () =
  print_int(leibniz_pi (-1.0));
  print_endline("");
  print_int(leibniz_pi (0.0000001));
  print_endline("")

let () = main ()