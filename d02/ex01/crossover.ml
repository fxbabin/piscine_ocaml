let contains elem =
  let rec aux = function
    | [] -> false
    | head :: tail ->
        if head = elem then true
        else aux tail
  in aux

let crossover lst1 lst2 =
  let rec aux acc = function
    | [] -> acc
    | head :: tail ->
        if (contains head lst2) && not (contains head acc) then aux (acc @ [head]) tail
        else aux acc tail
    in aux [] lst1 

let rec print_list =
      function
      | [] -> print_string("END \n")
      | head :: rest ->
        Printf.printf "%s; " head;
        print_list rest

let main () =
  print_endline(string_of_bool (contains "d" ["a"; "b"]));
  print_list(crossover ["c"; "a"] ["a"; "b"; "c"]);
  print_list(crossover [] ["a"; "b"; "c"]);
  print_list(crossover ["c"; "a"] [])

let () = main ()