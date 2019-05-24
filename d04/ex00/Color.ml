type t = Spade | Heart | Diamond | Club

let all = [Spade; Heart ; Diamond ; Club]

let toString card = match card with
    | Spade -> "S"
    | Heart -> "H"
    | Diamond -> "D"
    | Club -> "C"

let toStringVerbose card = match card with
    | Spade -> "Spade"
    | Heart -> "Heart"
    | Diamond -> "Diamond"
    | Club -> "Club"
