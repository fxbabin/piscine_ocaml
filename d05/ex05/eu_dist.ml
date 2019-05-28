
let eu_dist (a: float array) (b: float array) =
  let res = ref 0.0 in
  for i = 0 to ((Array.length a) - 1) do
    res := !res +. ((a.(i) -. b.(i)) *. (a.(i) -. b.(i)))
  done; sqrt !res

let () =
  print_float (eu_dist [|2.0; 1.0|] [|5.0; 3.0|]);
  print_endline ("")