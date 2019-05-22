let encode =
    let rec encode_aux nb acc = function
      | [] -> []
      | [e] -> acc @ [(nb + 1, e)]
      | prev :: (next :: rest as tail) ->
          if prev = next then encode_aux (nb + 1) acc tail
          else encode_aux 0 (acc @ [(nb + 1, prev)]) tail
    in encode_aux 0 []

let rec print_tuples =
  function
  | [] -> print_string("END \n")
  | (a, b) :: rest ->
    Printf.printf "%i, %s; " a b;
    print_tuples rest

let main () =
  print_tuples [(3, "toto");(1, "a")];
  print_tuples (encode []);
  print_tuples (encode ["a"]);
  print_tuples (encode ["a"; "a"]);
  print_tuples (encode ["a"; "a"; "a"]);
  print_tuples (encode ["a"; "a"; "a"; "a"]);
  print_tuples (encode ["a"; "a"; "a"; "a"; "b"; "b"; "d"])

let () = main ()