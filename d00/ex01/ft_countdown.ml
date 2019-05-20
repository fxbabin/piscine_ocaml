let rec ft_countdown num: unit =
  if num > 0 then begin
      print_int(num);
      print_char('\n');
      ft_countdown(num - 1);
  end
  else begin
    print_char('0');
    print_char('\n');
  end
;;

let main() =
ft_countdown(0);
ft_countdown(-1);
ft_countdown(3);
;;

let () = main ()