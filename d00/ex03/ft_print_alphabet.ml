let ft_print_alphabet () =
  let rec loop num =
    if num > int_of_char('z') then print_char('\n')
    else begin
        print_char(char_of_int(num));
        loop (num + 1)
    end
  in
  loop (int_of_char('a'))

let main() =
  ft_print_alphabet()

let () = main ()