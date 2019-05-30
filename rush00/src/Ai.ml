let possible_moves (game : Game.t) : (int * int) list =
  let valid_move x y =
    let bx, by = x / 3, y / 3 in
    let meta = Board.getcell (game.meta) bx by in
    if meta <> Cell.Empty
    then
      false
    else
      let cell = Game.getBigCell game.big x y in
      cell == Cell.Empty
  in
  let rec loop x y acc =
    if y > 8
    then
      acc
    else if x > 8
    then
      loop 0 (succ y) acc
    else if valid_move x y
    then
      loop (succ x) y ((x, y) :: acc)
    else
      loop (succ x) y acc
  in
  loop 0 0 []

let random_move (game : Game.t) : (int * int) =
  let moves = possible_moves game in
  List.nth moves (Random.int (List.length moves))

let eval_board (board : Board.t) (turn : Cell.t) (me : Cell.t) : int =
(*
    Point system:
          Lose game = -100000
          Adv 3 in row  = -2
          Adv 2 in row = -1
          neutral = 0
          Me 2 in row = +1
          Me 3 in row = +2
          Win = 100000
 *)
  let calc_score a b c =
    if a = b && b = c && a = me
    then
      15
    else if a = b && b = c && a <> me && a <> Empty
    then
      -15
    else if a <> me && a <> Empty && c = me
    then
      10
    else if a = c && a <> me && a <> Empty
    then
      -10
    else if a = b && a = me && c = Empty
    then
      5
    else if a = b && a <> me && a <> Empty && c = Empty
    then
      -5
    else
      0
  in
  let rec loop lst max_score =
    match lst with
      [] -> max_score
    | (a, b, c) :: xs -> let score = calc_score a b c in
                         let reversed = calc_score c b a in
                         let score = if abs score > abs reversed then score else reversed
                         in
                         if abs score > abs max_score
                         then
                           loop xs score
                         else
                           loop xs max_score
  in
  match board with
    a :: b :: c :: d :: e :: f :: g :: h :: i :: [] -> let lines = [(a, b, c);
                                                                    (d, e, f);
                                                                    (g, h, i);
                                                                    (a, d, g);
                                                                    (b, e, h);
                                                                    (c, f, i);
                                                                    (a, e, i);
                                                                    (c, e, g)
                                                                   ]
                                                       in
                                                       loop lines 0
  | _ -> 0

let eval (game : Game.t) (me : Cell.t) : int =
  let winner = Board.has_winner game.meta game.turn in
  if winner <> Empty
  then
    if winner <> me then (-1000000) else 1000000
  else
    let tmp = List.fold_left (fun a b -> a + (eval_board b game.turn me)) 0 game.big in
    tmp + (eval_board game.meta game.turn me) * 3

let minmax (input : Game.t) : (int * int) =
  let rec _minmax (game : Game.t) (depth : int) : int =
    if depth = 0 || ((Board.has_winner game.meta game.turn) <> Empty)
    then
      eval game input.turn
    else
      let value = if game.turn = input.turn then (-10000) else 10000 in
      let f = if game.turn = input.turn then max else min in
      let moves = possible_moves game in
      List.fold_left
        (fun a b -> f a
                      (_minmax
                         (Game.play_move game (fst b) (snd b))
                         (pred depth)))
        value moves
  in
  let rec loop (lst : (int * int) list) (max_val : (int * int)) (max_score : int) : (int * int) =
    match lst with
      [] -> max_val
    | move :: xs -> let sim_move = Game.play_move input (fst move) (snd move) in
                    let score = _minmax sim_move 2 in
                    (*print_endline (string_of_int score);*)
                    if score > max_score
                    then
                      loop xs move score
                    else
                      loop xs max_val max_score
  in
  let moves = possible_moves input in
  loop moves (0, 0) (-1000000)
