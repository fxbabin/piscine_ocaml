let encode =
  let rec encode_aux nb acc = function
    | [] -> []
    | [e] -> acc @ [string_of_int (nb + 1); e]
    | prev :: (next :: rest as tail) ->
        if prev = next then encode_aux (nb + 1) acc tail
        else encode_aux 0 (acc @ [string_of_int (nb + 1); prev]) tail
  in encode_aux 0 []

let list_to_string =
  let rec aux acc = function
    | [] -> ""  
    | [e] -> acc ^ e
    | head :: tail -> aux (acc ^ head) tail
  in aux ""

let sequence n = 
  let rec aux n = 
    if n <= 0 then [""]
    else if n == 1 then ["1"]
    else encode (aux (n - 1))
  in list_to_string (aux n)

let main () =
  print_endline(sequence (-1));
  print_endline(sequence 0);
  print_endline(sequence 2);
  print_endline(sequence 3);
  print_endline(sequence 4);
  print_endline(sequence 5);
  print_endline(sequence 6);
  print_endline(sequence 7)

let () = main ()