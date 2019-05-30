type t = Cell.t list

let toString (board : t) : string list =
  let rec loop (lst : t) : string list =
    match lst with
      [] -> []
    | x :: y :: z :: xs -> ((Cell.toString x) ^ " " ^ (Cell.toString y) ^ " " ^ (Cell.toString z)) :: (loop xs)
    | _ -> ["E"]
  in
  loop board

let has_winner (board : t) (last : Cell.t) : Cell.t =
  let rec check_comb lst =
    match lst with
      x :: y :: z :: [] -> if x = y && y = z
                           then
                             x
                           else
                             Cell.Empty
    | _ -> failwith "Comb checking"
  in
  let rec gen_line board =
    match board with
      [] -> []
    | x :: y :: z :: xs -> [x; y; z] :: (gen_line xs)
    | _ -> failwith "Line generation"
  in
  let rec gen_diag board =
    match board with
      a :: b :: c :: d :: e :: f :: g :: h :: i :: [] -> [[a; e; i]; [c; e; g]]
    | _ -> failwith "Diag generation"
  in
  let rec gen_column board =
    match board with
      a :: b :: c :: d :: e :: f :: g :: h :: i :: [] -> [[a; d; g]; [b; e; h]; [c; f; i]]
    | _ -> failwith "Col generation"
  in
  let rec full board =
    match board with
      [] -> true
    | x :: xs -> if x = Cell.Empty
                 then
                   false
                 else
                   full xs
  in
  let rec loop lst =
    match lst with
      [] -> Cell.Empty
    | x :: xs -> let tmp = check_comb x in
                 if tmp <> Empty
                 then
                   tmp
                 else
                   loop xs
  in
  let combs =  (gen_line board) @ (gen_diag board) @ (gen_column board) in
  let winner = loop combs in
  if winner = Empty && (full board)
  then
    last
  else
    winner

let setcell (board: t) x y (cell: Cell.t) : t =
  let lim_x = 2 in
  let lim_y = 2 in
  let ref_idx =
    if x < 0 || y < 0 || y > lim_y || x > lim_x then raise (Failure "setcell")
    else (y * 3 + x)
  in
  let rec aux idx acc = function
    | [] -> acc
    | head :: tail ->
       if idx = ref_idx then aux (idx + 1) (acc @ [cell]) tail
       else aux (idx + 1) (acc @ [head]) tail
  in aux 0 [] board

let getcell (board: t) x y : Cell.t =
  let lim_x = 2 in
  let lim_y = 2 in
  let ref_idx =
    if x < 0 || y < 0 || y > lim_y || x > lim_x then raise (Failure "getcell")
    else (y * 3 + x)
  in
  let rec aux idx = function
    | [] -> raise (Failure "getcell1")
    | head :: tail ->
      if idx = ref_idx then head
      else aux (idx + 1) tail
  in aux 0 board
