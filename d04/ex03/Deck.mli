type t

val newDeck : t

val toStringList : t -> string list
val toStringListVerbose : t -> string list
val drawCard : t -> (Card, t)
