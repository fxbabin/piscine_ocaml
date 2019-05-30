type t =
  {
    p1_name : string;
    p1_read : t -> (int * int);
    p2_name : string;
    p2_read : t -> (int * int);
    big : Board.t list;
    meta : Board.t;
    print_func : t -> unit;
    turn : Cell.t
  }

val read_user : t -> (int * int)

val print_game : t -> unit
val full : t -> unit
val play_turn : t -> Cell.t


val getBigCell : Board.t list -> int -> int -> Cell.t
val setBigCell : Board.t list -> int -> int -> Cell.t -> Board.t list


val play_move : t -> int -> int -> t
