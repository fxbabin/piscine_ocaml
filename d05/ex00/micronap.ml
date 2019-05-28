(* ocamlc unix.cma micronap.ml -o micronap *)

let my_sleep () = Unix.sleep 1

let () =
  try 
    if Array.length Sys.argv <> 2 then failwith "wrong number of arguments\n";
    for i = 1 to int_of_string(Sys.argv.(1)) do
      my_sleep ();
    done
  with e -> match e with
    | Not_found -> print_string("Not found")
    | Failure err -> exit 1
    | _ -> print_string("???")