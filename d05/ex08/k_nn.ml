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

let compare_tuple (tup1: float * string) (tup2: float * string) =
  if (fst tup1) > (fst tup2) then 1
  else if (fst tup1) < (fst tup2) then -1
  else 0

let compare_string s1 s2 =
  if s1 > s2 then 1
  else if s1 < s2 then -1
  else 0

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
  
let k_nn (tuples: radar list) k (radar: radar): string =
  let acc = ref [] in
  let rad = fst radar in
  for i = 0 to ((List.length tuples) - 1) do
      let curr = fst (List.nth tuples i) in
      let currn = snd (List.nth tuples i) in
      acc := !acc @ [((eu_dist rad curr), currn)]
  done;
  let sorted_dist = List.sort compare_tuple !acc in
  let acc = ref [] in
  for i = 0 to (k - 1) do
      acc := !acc @ [snd (List.nth sorted_dist i)];
  done; 
  let ss = List.sort compare_string !acc in
  let x = encode ss in
  let max = ref 0 in
  let max_name = ref "" in
  for i = 0 to ((List.length x) - 1) do
    let d = fst (List.nth x i) in
    if d > !max then
      begin
        max := d;
        max_name := (snd (List.nth x i));
      end
done; !max_name

let () =
  let filename = get_filename () in
  let tuples = examples_of_file filename in
  let a = k_nn tuples 1 ([|0.0; 0.2; 0.1|], "a") in
  print_string ("a -> ");
  print_endline(a);
  let b = k_nn tuples 1 ([|0.3; 0.4; 0.5|], "b") in
  print_string ("b -> ");
  print_endline(b);
  let c = k_nn tuples 1 ([|0.9; 0.5; 0.8|], "c") in
  print_string ("c -> ");
  print_endline(c);
  let a = k_nn tuples 3 ([|0.0; 0.2; 0.1|], "a") in
  print_string ("a -> ");
  print_endline(a);
  let b = k_nn tuples 3 ([|0.3; 0.4; 0.5|], "b") in
  print_string ("b -> ");
  print_endline(b);
  let c = k_nn tuples 3 ([|0.9; 0.5; 0.8|], "c") in
  print_string ("c -> ");
  print_endline(c)