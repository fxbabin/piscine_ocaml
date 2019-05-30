class people = 
    object
        val _name:string = "Amy Pond"
        val _hp:int = 100

        initializer print_endline ("Amy Pond is born")
        method get_name () = _name
        method to_string () = "name : " ^ _name ^ "\thp : " ^ (string_of_int _hp)
        method announce () = print_endline ("I'm " ^ _name ^ "! Do you know the Doctor ?")
        method die () = print_endline ("Aaaarghh!")

    end
