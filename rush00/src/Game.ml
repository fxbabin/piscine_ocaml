type t =
  {
    p1_name : string;
    p1_read : t -> (int * int);
    p2_name : string;
    p2_read : t -> (int * int);
    big : Board.t list;
    meta : Board.t;
    print_func: t -> unit;
    turn : Cell.t
  }

let rec print_list =
  function
  | [] -> print_string("END \n")
  | head :: rest ->
     Printf.printf "%s; " head;
     print_list rest

let full (game: t) = 
  Graphics.clear_graph ();
  let draw_x x y size =
    Graphics.draw_rect (x - (size / 2)) (y - (size / 2)) size size;
    Graphics.set_line_width 3;
    Graphics.set_color Graphics.green;
    Graphics.moveto (x - (size / 4)) (y - (size / 4));
    Graphics.lineto (x + (size / 4)) (y + (size / 4));
    Graphics.moveto (x - (size / 4)) (y + (size / 4));
    Graphics.lineto (x + (size / 4)) (y - (size / 4));
    Graphics.set_color Graphics.black;
    Graphics.set_line_width 1
  in
  let draw_o x y size =
    Graphics.draw_rect (x - (size / 2)) (y - (size / 2)) size size;
    Graphics.set_line_width 3;
    Graphics.moveto x y;
    Graphics.set_color Graphics.red;
    Graphics.draw_circle x y (size / 3);
    Graphics.set_color Graphics.black;
    Graphics.set_line_width 1
  in
  let draw_e x y size =
    Graphics.draw_rect (x - (size / 2)) (y - (size / 2)) size size
  in
  let get_drawcell e x y size = match e with
    | Cell.X -> draw_x x y size
    | Cell.O -> draw_o x y size
    | _ -> draw_e x y size
  in
  let draw_board x_base y_base size =
    let rec aux x y = function
      | [] -> ()
      | head :: tail -> 
        if x = (x_base + (size * 2)) then (
            get_drawcell head x y size;
            aux x_base (y - size) tail
        ) 
        else (
          get_drawcell head x y size;
          aux (x + size) y tail
        )
    in aux x_base y_base
  in
  let print_e e fullboard idx x y size =
    if e <> Cell.Empty then
      get_drawcell e (x + size) (y - size) (size * 3)
    else
      draw_board x y size (List.nth fullboard idx)
  in
  let meta = game.meta in 
  let fullboard = game.big in
  let x_base = 50 in
  let y_base = 475 in
  let size = 50 in
  let rec aux (x:int) (y:int) idx = function
  | [] -> ()
  | h::t -> 
      if x >= 380 then (
        print_e h fullboard idx x y size;
        aux x_base (y - 165) (idx + 1) t
      )
      else (
        print_e h fullboard idx x y size;
        aux (x + 165) y (idx + 1) t
      )
  in aux x_base y_base 0 meta

let print_game (game : t) : unit =
  let bigX = [
      "\027[32m\\   /\027[0m";
      "\027[32m  X  \027[0m";
      "\027[32m/   \\\027[0m"]
  in
  let bigO = [
      "\027[31m/ - \\\027[0m";
      "\027[31m|   |\027[0m";
      "\027[31m\\ - /\027[0m"]
  in
  let print_board_line board line =
    let m_line = 3 * (line / 3) in
    let mm_line = line mod 3 in
    print_string(List.nth (List.nth board m_line) mm_line);
    print_string(" | ");
    print_string(List.nth (List.nth board (m_line + 1)) mm_line);
    print_string(" | ");
    print_string(List.nth (List.nth board (m_line + 2)) mm_line);
    print_string("\n")
  in
  let print_board board =
    print_board_line board 0;
    print_board_line board 1;
    print_board_line board 2;
    print_endline("---------------------");
    print_board_line board 3;
    print_board_line board 4;
    print_board_line board 5;
    print_endline("---------------------");
    print_board_line board 6;
    print_board_line board 7;
    print_board_line board 8
  in
  let transform i e =
    let x, y = i mod 3, i / 3 in
    let player = Board.getcell game.meta x y in
    if player = O
    then
      bigO
    else if player = X
    then
      bigX
    else
      Board.toString e
  in
  print_board @@ List.mapi transform game.big

let getBigCell (boards : Board.t list) (x : int) (y : int) : Cell.t =
  Board.getcell (List.nth boards ((y / 3) * 3 + (x / 3))) (x mod 3) (y mod 3)

let setBigCell (boards : Board.t list) (x : int) (y : int) (value : Cell.t) : Board.t list =
  let idx = (y / 3) * 3 + (x / 3) in
  let bx, by = x mod 3, y mod 3 in
  let rec loop lst i acc =
    match lst with
      [] -> acc
    | hd :: tl -> let tmp = if i = idx then Board.setcell hd bx by value else hd in
                  loop tl (succ i) (tmp :: acc)
  in
  List.rev (loop boards 0 [])

let rec read_user (game : t) : (int * int) =
  let valid_input game x y =
    let bx, by = x / 3, y / 3 in
    let meta = Board.getcell (game.meta) bx by in
    let cell = getBigCell game.big x y in
    cell == Empty && meta == Empty
  in
  let parse_input str =
    let rec is_int s i len =
      if i = len
      then
        true
      else if s.[i] < '1' || s.[i] > '9'
      then
        false
      else
        is_int s (succ i) len
    in
    let input = List.filter (fun x -> x <> "") (String.split_on_char ' ' str) in
    if List.length input != 2
    then
      (-1), (-1)
    else (
      let str_y, str_x = List.nth input 0, List.nth input 1 in
      if not (is_int str_x 0 (String.length str_x)) || not (is_int str_y 0 (String.length str_y))
      then
        (-1), (-1)
      else
        int_of_string str_x - 1, int_of_string str_y - 1 )
  in
  let str = read_line () in
  let x, y = parse_input str in
  if x > 9 || x < 0 || y > 9 || y < 0
  then (
    print_endline "Incorrect format";
    read_user game )
  else if not (valid_input game x y)
  then (
    print_endline "Illegal move";
    read_user game )
  else
    (x, y)


let play_move (game : t) (x : int) (y : int) : t =
  let check_meta (player : string) (meta : Board.t) (subboard : Board.t) (turn : Cell.t) (x : int) (y : int) : Board.t =
    let winner = Board.has_winner subboard turn in
    if winner <> Empty
    then
      (*print_endline @@ player ^ " wins the board " ^ (string_of_int (y * 3 + x)) ^ "!"; *)
      Board.setcell meta x y winner
    else
      meta
  in
  let player = if game.turn = O then game.p1_name else game.p2_name in
  let newbig = setBigCell game.big x y game.turn in
  let newmeta = check_meta player game.meta (List.nth newbig ((y / 3) * 3 + (x / 3))) game.turn (x / 3) (y / 3) in
  let next_turn = if game.turn = X then Cell.O else Cell.X in
  let newgame = {
      p1_name = game.p1_name;
      p1_read = game.p1_read;
      p2_name = game.p2_name;
      p2_read = game.p2_read;
      big = newbig;
      meta = newmeta;
      print_func = game.print_func;
      turn = next_turn
    }
  in
  newgame

let rec play_turn (game : t) : Cell.t =
  game.print_func game;
  print_char '\n';
  let player = if game.turn = O then game.p1_name else game.p2_name in
  print_endline @@ player ^ "'s turn to play:";
  let read_f = if game.turn = O then game.p1_read else game.p2_read in
  let x, y = read_f game in
  let newgame = play_move game x y in
  let winner = Board.has_winner newgame.meta game.turn in
  if winner <> Empty
  then (
    print_endline @@ player ^ " wins the game!";
    game.print_func newgame;
    winner )
  else
    play_turn newgame






