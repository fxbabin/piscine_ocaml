let rec print_cards_string lst = match lst with
    | [] -> print_char '\n'
    | head::tail -> print_string (Color.toString head); print_string " " ;print_cards_string tail

let rec print_cards_string_verbose lst = match lst with
    | [] -> print_char '\n'
    | head::tail -> print_string (Color.toStringVerbose head); print_string " " ;print_cards_string_verbose tail

let main () = 
    let lst = Color.all in
    print_cards_string lst;
    print_cards_string_verbose lst

let () = main ()
