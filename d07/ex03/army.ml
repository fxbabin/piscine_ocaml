class ['a] army =
    object
        val mutable _arm: ('a list) = []

        method add (elem:'a) = 
            _arm <- (elem :: _arm)

        method delete () =
            let remove lst = match lst with
                | [] -> ()
                | head::tail -> _arm <- tail
            in remove _arm

        method print () = 
            let rec loop lst = match lst with
                | [] -> print_endline ""
                | head::tail -> print_endline (head#to_string ()); loop tail
            in loop _arm
    end
