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

let eu_dist (a: float array) (b: float array) =
  let res = ref 0.0 in
  for i = 0 to ((Array.length a) - 1) do
    res := !res +. ((a.(i) -. b.(i)) *. (a.(i) -. b.(i)))
  done; sqrt !res

type radar = (float array) * string

let one_nn (tuples: radar list)  (radar: radar): string =
  let min = ref 2147486647.0 in
  let min_name = ref "" in
  let rad = fst radar in
  for i = 0 to ((List.length tuples) - 1) do
     let curr = fst (List.nth tuples i) in
     let currn = snd (List.nth tuples i) in
     let dist = eu_dist rad curr in
     if dist < !min then
      begin
        min := dist;
        min_name := currn;
      end
   done; !min_name

let () =
  let filename = get_filename () in
  let tuples = examples_of_file filename in
  let a = one_nn tuples ([|0.0; 0.2; 0.1|], "a") in
  print_string ("a -> ");
  print_endline(a);
  let b = one_nn tuples ([|0.3; 0.4; 0.5|], "b") in
  print_string ("b -> ");
  print_endline(b);
  let c = one_nn tuples ([|0.9; 0.5; 0.8|], "c") in
  print_string ("c -> ");
  print_endline(c)