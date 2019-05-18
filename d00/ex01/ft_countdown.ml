let rec ft_countdown num =
  if num > 0
  then
    begin
      print_int(num);
      print_endline("");
      ft_countdown(num - 1);
    end
  else print_endline("0");
;;

let main() =
ft_countdown(0);
ft_countdown(-1);
ft_countdown(3);
;;

let () = main ()