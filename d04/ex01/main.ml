let rec print_ints lst = match lst with
    | [] -> print_char '\n'
    | head::tail -> print_int (Value.toInt head); print_char ' '; print_ints tail

let rec print_strings lst = match lst with
    | [] -> print_char '\n'
    | head::tail -> print_string (Value.toString head); print_char ' '; print_strings tail

let rec print_stringsVerbose lst = match lst with
    | [] -> print_char '\n'
    | head::tail -> print_string (Value.toStringVerbose head); print_char ' '; print_stringsVerbose tail

let rec print_nexts lst = match lst with
    | [] -> print_char '\n'
    | head::tail when head = Value.As -> print_string "/!\\ As has no next /!\\"; print_char ' '; print_nexts tail
    | head::tail -> print_string (Value.toString (Value.next head)); print_char ' '; print_nexts tail

let rec print_previouss lst = match lst with
    | [] -> print_char '\n'
    | head::tail when head = Value.T2 -> print_string "/!\\ 2 has no previous /!\\"; print_char ' '; print_previouss tail
    | head::tail -> print_string (Value.toString (Value.previous head)); print_char ' '; print_previouss tail

let main () =
    let lst = Value.all in
    print_ints lst;
    print_strings lst;
    print_stringsVerbose lst;
    print_nexts lst;
    print_previouss lst

let () = main ()
