let mat () = 
  Random.self_init ();
  let rec aux x = match x with
    | n when (n >= 97 && n <= 122) -> char_of_int(x)
    | _ -> aux (Random.int 120)
  in aux (Random.int 120)

let rand_people () = 
  let rec auxx acc idx = match idx with
    | n when n >= 9 -> acc
    | _ -> auxx (acc ^ (String.make 1 (mat ()))) (idx + 1)
  in auxx (String.make 1 (char_of_int (int_of_char (mat()) - 32))) 0

class people = 
    object
        val _name:string = rand_people ()
        val _hp:int = 100

        initializer print_endline (_name ^ " is born")
        method get_name () = _name
        method to_string () = "name : " ^ _name ^ "\thp : " ^ (string_of_int _hp)
        method announce () = print_endline ("I'm " ^ _name ^ "! Do you know the Doctor ?")
        method die () = print_endline ("Aaaarghh!")

    end
