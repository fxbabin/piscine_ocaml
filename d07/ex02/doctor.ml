class doctor =
    object
        val _name:string = "Who?"
        val mutable _age:int = 1103
        val _sidekick:People.people = new People.people
        val mutable _hp = 100

        initializer print_endline "The 11th doctor arrived, aged of 1103 years"

        method get_sidekick () = _sidekick
        method to_string () = "name : " ^ _name ^ "\nage : " ^ (string_of_int _age) ^ "\nsidekick : " ^ (_sidekick#to_string ()) ^ "\nhp : "^ (string_of_int _hp)
        method talk () = print_endline "Hi! I’m the Doctor!"
        method travel_in_time (start:int) (arrival:int) = 
            print_string "
        ___
_______(_@_)_______
| POLICE      BOX |
|_________________|
 | _____ | _____ |
 | |###| | |###| |
 | |###| | |###| |   
 | _____ | _____ |   
 | || || | || || |
 | ||_|| | ||_|| |  
 | _____ |$_____ |  
 | || || | || || |  
 | ||_|| | ||_|| | 
 | _____ | _____ |
 | || || | || || |   
 | ||_|| | ||_|| |         
 |       |       |        
 *****************\n";
            let diff = (arrival - start) in
                _age <- _age + diff
		method use_sonic_screwdriver () = print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"
		method private regenerate () = _hp <- 100

    end
