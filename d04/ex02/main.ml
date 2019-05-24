let compare_two_cards f =
    let card1 = Card.newCard Card.Value.Queen Card.Color.Spade
    in let card2 = Card.newCard Card.Value.King Card.Color.Heart
    in f card1 card2

let rec print_stringVerbose_cards lst = match lst with 
    | [] -> print_char '\n'
    | head::tail -> print_string (Card.toStringVerbose head) ; print_char ' ' ; print_stringVerbose_cards tail

let rec print_string_cards lst = match lst with 
    | [] -> print_char '\n'
    | head::tail -> print_string (Card.toString head) ; print_char ' ' ; print_string_cards tail

let main () = 
    let all = Card.all in
    print_string_cards all;
    print_stringVerbose_cards all;
    print_int (compare_two_cards Card.compare);
    print_char '\n';
    print_endline (Card.toString (compare_two_cards Card.max));
    print_endline (Card.toString (compare_two_cards Card.min));
    print_endline (Card.toString (Card.best [(Card.newCard Card.Value.Queen Card.Color.Spade);(Card.newCard Card.Value.King Card.Color.Heart)]));
    print_endline (string_of_bool (Card.isSpade (Card.newCard Card.Value.Queen Card.Color.Spade)));
    print_endline (string_of_bool (Card.isHeart (Card.newCard Card.Value.Queen Card.Color.Spade)));
    print_endline (string_of_bool (Card.isDiamond (Card.newCard Card.Value.Queen Card.Color.Spade)));
    print_endline (string_of_bool (Card.isClub (Card.newCard Card.Value.Queen Card.Color.Spade)))

let () = main ()
