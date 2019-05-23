let reverse_list =
  let rec aux acc = function
    | [] -> []  
    | [e] -> e :: acc
    | head :: tail -> aux (head :: acc) tail
  in aux []

let concat_list_gray lst1 lst2 =
  let rec aux acc c = function
    | [] -> []  
    | [e] -> (c ^ e) :: acc
    | head :: tail -> aux ((c ^ head) :: acc) c tail
  in let accc = aux [] "0" lst1 
  in reverse_list (aux accc "1" lst2)

let list_to_string =
  let rec aux acc = function
    | [] -> " "  
    | [e] -> e ^ acc
    | head :: tail -> aux ((" " ^ head) ^ acc) tail
  in aux ""

let gray n =
  let rec aux n =
    if n <= 0 then []
    else if n == 1 then ["0"; "1"]
    else concat_list_gray (aux (n - 1)) (reverse_list (aux (n - 1)))
  in print_endline (list_to_string (reverse_list (aux n)))

let main () =
  gray (-1);
  gray 0;
  gray 1;
  gray 2;
  gray 3;
  gray 4

let () = main ()