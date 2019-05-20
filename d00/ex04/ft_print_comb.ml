let print_comb i j k =
  if i == 7 && j == 8 && k == 9 then begin
    print_int(i);
    print_int(j);
    print_int(k);
    print_string("\n");
  end
  else if i < j && j < k then begin
    print_int(i);
    print_int(j);
    print_int(k);
    print_string(", ");
  end

let ft_print_comb () =
  let rec loop i j k =
    if i == 8 then ()
    else if k == 10 then loop i (j + 1) 0
    else if j == 10 then loop (i + 1) 0 k
    else begin
      print_comb i j k;
      loop i j (k + 1);
    end
  in
  loop 0 0 0;
;;

let main() =
  ft_print_comb ();
;;

let () = main ()