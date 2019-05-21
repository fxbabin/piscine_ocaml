let print_comb2 i j k l: unit =
  if i == 9 && j == 8 && k == 9 && l == 9 then begin
    print_int(i);
    print_int(j);
    print_char(' ');
    print_int(k);
    print_int(l);
    print_char('\n');
  end
  else if (i * 10 + j) < (k * 10 + l) then begin
    print_int(i);
    print_int(j);
    print_char(' ');
    print_int(k);
    print_int(l);
    print_char(',');
    print_char(' ');
  end

let ft_print_comb2 (): unit =
  let rec loop i j k l =
    if i == 9 && j == 9 then ()
    else if j == 10 then loop (i + 1) 0 (i + 1) 1
    else if k == 10 then loop i (j + 1) i (j + 1)
    else if l == 10 then loop i j (k + 1) 0
    else begin
      print_comb2 i j k l;
      loop i j k (l + 1);
    end
  in
  loop 0 0 0 1

let main() =
  ft_print_comb2 ()

let () = main ()