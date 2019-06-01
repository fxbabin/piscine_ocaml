let () =
  let h1:Watchtower.Watchtower.hour = 8 in
  let h2:Watchtower.Watchtower.hour = 5 in
  print_string("1 -> ");
  let sum:Watchtower.Watchtower.hour = Watchtower.Watchtower.add h1 h2 in
  print_endline(string_of_int sum);
  print_string("3 -> ");
  let subs:Watchtower.Watchtower.hour = Watchtower.Watchtower.sub h1 h2 in
  print_endline(string_of_int subs);
  print_string("9 -> ");
  print_endline(string_of_int (Watchtower.Watchtower.sub h2 h1));
  let h1:Watchtower.Watchtower.hour = 18 in
  let h2:Watchtower.Watchtower.hour = 5 in
  print_string("11 -> ");
  print_endline(string_of_int (Watchtower.Watchtower.sub h2 h1));
  ()