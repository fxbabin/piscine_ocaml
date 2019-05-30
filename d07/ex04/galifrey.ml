let create_dalek_army n =
    let rec loop army nb = match nb with 
        | 0 -> army
        | _ -> army#add (new Dalek.dalek );loop army (nb - 1)
    in loop (new Army.army) n

let create_people_army n =
    let rec loop army nb = match nb with 
        | 0 -> army
        | _ -> army#add (new People.people );loop army (nb - 1)
    in loop (new Army.army) n

let kill_all_dalek army doctor =
    let rec loop () = match army#get_army () with 
        | [] -> ()
        | head::tail -> doctor#use_sonic_screwdriver (); head#die (); army#delete (); loop ()
    in loop ()

let kill_one_victim army victims =
    let killeur_index = Random.int 9 in
    let killeur = List.nth (army#get_army ()) killeur_index in
        match victims#get_army () with
            | [] -> ()
            | head::tail -> killeur#exterminate head; victims#delete ();

class galifrey =
    object
        val mutable _dalek: (Dalek.dalek list) = []
        val mutable _doctor: (Doctor.doctor list) = []
        val mutable _people: (People.people list) = []
    
        method do_time_war () = 
            Random.self_init();
            let army = create_dalek_army 10 in
            let victims = create_people_army 50 in
            begin 
            let rec loop nb = match Random.int 50 with
                | 1 -> let doctor = (new Doctor.doctor) in doctor#travel_in_time 0 0; kill_all_dalek army doctor; print_endline "Time lords won the time war ! but Galifrey had been destroyed, that's an heavy tribute, but they had no choice"
                | _ when nb = 50 -> print_endline "The Daleks won the time war ! who knows the future of the universe now ? "
                | _ -> kill_one_victim army victims; loop (nb + 1)
            in loop 0
            end
    end
