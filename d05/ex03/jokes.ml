
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

let () =
  let filename = get_filename () in
  let lines = read_flines filename in
  let nb_lines = (List.length lines) in
  let joke_arr = Array.make nb_lines "" in
  for i = 0 to (nb_lines - 1) do
    joke_arr.(i) <- List.nth lines i
  done;
  if nb_lines = 0 then
    ()
  else begin
    Random.self_init ();
    print_endline(Array.get joke_arr (Random.int nb_lines))
  end