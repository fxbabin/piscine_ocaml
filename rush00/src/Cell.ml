type t = Empty | O | X

let toString (cell : t) : string =
  match cell with
    Empty -> "-"
  | O -> "\027[31mO\027[0m"
  | X -> "\027[32mX\027[0m"
