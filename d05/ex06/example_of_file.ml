let get_filename () = 
  try
    let op = open_in Sys.argv.(1) in
    close_in op;
    Sys.argv.(1)
  with 
   | Not_found -> exit 1
   | _ -> exit 1

let read_flines file: string list =
  let in_file = open_in file in
  let arr = ref [] in
  try
    while true do
      let line = input_line in_file in
      arr := !arr @ [line]
    done; !arr
  with
    | End_of_file -> !arr
    | _ -> []

let print_tup (f_arr, str) =
  for i = 0 to ((Array.length f_arr) - 1) do
    print_float f_arr.(i) ; print_string " "
  done;
  print_endline str

let extract_tuple line =
  let s = String.split_on_char ',' line in
  let arr = Array.make (List.length s) 0.0 in
  for i = 0 to ((List.length s) - 2) do
    arr.(i) <- float_of_string (List.nth s i)
  done;
  (arr, (List.nth s ((List.length s) - 1)))

let examples_of_file path : (float array * string) list =
  let lines = read_flines path in
  let arr = ref [] in
  for i = 0 to ((List.length lines) - 1) do
    arr := !arr @ [extract_tuple (List.nth lines i)]
  done;
  !arr

let () =
  let filename = get_filename () in
  let tuples = examples_of_file filename in
  for i = 0 to ((List.length tuples) - 1) do
    print_tup (List.nth tuples i)
  done