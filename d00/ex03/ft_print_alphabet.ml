let ft_print_alphabet () =
  let rec loop num =
    if num > 122 then print_char('\n')
    else begin
        print_char(char_of_int(num));
        loop (num + 1)
    end
  in
  loop 97

let main() =
  ft_print_alphabet();
;;

let () = main ()