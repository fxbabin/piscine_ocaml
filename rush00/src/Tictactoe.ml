let rec read_player (n : int) =
  let rec is_valid str i len =
    if len = 0
    then
      false
    else if i = len
    then
      true
    else
      let c = str.[i] in
      if not ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9'))
      then
        false
      else
        is_valid str (succ i) len
  in
  print_string @@ "Please enter player " ^ (string_of_int n) ^ "'s name: ";
  let tmp = read_line () in
  if is_valid tmp 0 (String.length tmp)
  then
    tmp
  else (
    print_endline "Sorry, I can't allow this type of name.";
    read_player n )

let set_player name =
  match name with
    "random" -> Ai.random_move
  | "minmax" -> Ai.minmax
  | _ -> Game.read_user

let () =
  let print_func =
  if Array.length Sys.argv = 2 && Sys.argv.(1) = "-v"
  then (
    Graphics.open_graph " 530x530";
    Game.full )
  else
    Game.print_game
  in
  let rec loop game =
    ignore (Game.play_turn game);
    print_string "Wanna play again ? [y/N]: ";
    let ans = read_line () in
    match ans with
      "Y" | "y" | "O" | "o" | "Oui" | "Yes" | "Da" -> loop game
      | _ -> print_endline "Bye !"
  in
  Random.self_init ();
  print_endline "Welcome to Tictactoe !";
  print_endline "Available AIs :\n- random\n- minmax";
  let p1_name = read_player 1 in
  let p2_name = read_player 2 in
  let p1_read = set_player p1_name in
  let p2_read = set_player p2_name in
  let p1_name = "\027[31m" ^ p1_name ^ "\027[0m" in
  let p2_name = "\027[32m" ^ p2_name ^ "\027[0m" in
  let (game : Game.t) =
    { p1_name = p1_name;
      p1_read = p1_read;
      p2_name = p2_name;
      p2_read = p2_read;
      print_func = print_func;
      big = List.init 9 (fun _ -> List.init 9 (fun _ -> Cell.Empty));
      meta = List.init 9 (fun _ -> Cell.Empty);
      turn = O
    }
  in
  loop game
