let ft_print_rev str: unit =
  let rec loop str_len =
    if str_len < 0 then print_char('\n')
    else begin
      print_char(String.get str str_len);
      loop (str_len - 1);
    end
  in
  loop ((String.length str) - 1);
;;

let main() =
  ft_print_rev "Hello world !";
  ft_print_rev "";
;;

let () = main ()