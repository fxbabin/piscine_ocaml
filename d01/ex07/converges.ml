let rec converges f x n = 
  if n < 0 then false
  else if x == (f x) then true
  else converges f (f x) (n - 1)

let main () =
  print_endline(string_of_bool(converges (( * ) 2) 2 5));
  print_endline(string_of_bool(converges (fun x -> x / 2) 2 3));
  print_endline(string_of_bool(converges (fun x -> x / 2) 2 2));
  print_endline(string_of_bool(converges (fun x -> x * x) 1 2))

let () = main ()