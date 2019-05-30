class people = 
    object
        val _name:string = "Random"
        val _hp:int = 100

        initializer print_endline ("A new people named " ^ _name ^ " is born")
        method to_string () = "name : " ^ _name ^ "\nhp : " ^ (string_of_int _hp)
        method announce () = print_endline ("I'm " ^ _name ^ "! Do you know the Doctor ?")
        method die () = print_endline ("Aaaarghh!")
    end
