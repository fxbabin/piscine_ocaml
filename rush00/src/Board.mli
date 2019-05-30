type t = Cell.t list

val setcell : t -> int -> int -> Cell.t -> t
val getcell : t -> int -> int -> Cell.t

val has_winner : t -> Cell.t -> Cell.t

val toString : t -> string list
