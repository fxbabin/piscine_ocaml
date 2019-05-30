let mat () = 
  Random.self_init ();
  let rec aux x = match x with
    | n when (n >= 65 && n <= 90) || (n >= 97 && n <= 122) -> char_of_int(x)
    | _ -> aux (Random.int 123)
  in aux (Random.int 123)

let matricule () = 
  let rec auxx acc idx = match idx with
    | n when n >= 3 -> acc
    | _ -> auxx (acc ^ (String.make 1 (mat ()))) (idx + 1)
  in auxx "" 0

let sentences = [|"Explain! Explain!"; "Exterminate! Exterminate!"; "I obey!"; "You are the Doctor! You are the enemy of the Daleks!" |]

class dalek =
    object
        val mutable _name:string = "Dalek" ^ (matricule ())
        val _hp:int = 100
        val mutable _shield:bool = true

        method get_name () = _name
        method to_string () = "name : " ^ _name ^ "\nhp : " ^ (string_of_int _hp) ^ "\nshield : " ^ (string_of_bool _shield)
        method talk () = 
            let index = Random.int 4 in
            print_endline (sentences.(index))
        method exterminate (victim:People.people) =
            _shield <- not _shield;
            print_string (_name ^ " : Exterminate !!!" ^ "\n" ^ (victim#get_name ()) ^ " : Nooooo... ");
            victim#die ();

        method die () = print_endline "Emergency Temporal Shift!"
    end
