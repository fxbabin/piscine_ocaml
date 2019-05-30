let main () = 
    let scaro = new Dalek.dalek in 
    let doctor = new Doctor.doctor in
    let amy = doctor#get_sidekick () in
    begin
        print_endline "";
        print_endline "Presentation des protagonistes : ";
        print_endline (doctor#to_string ());
        print_endline "";
        print_endline (amy#to_string ());
        print_endline "";
        print_endline (scaro#to_string ());
        doctor#travel_in_time 10 20;
        print_endline "";
        amy#announce ();
        doctor#talk ();
        print_endline "";
        scaro#talk ();
        scaro#talk ();
        scaro#talk ();
        print_endline "";
        scaro#exterminate (amy);
        print_endline "";
        print_endline (scaro#to_string ());
        print_endline "* The shield of the dalek is unactive *";
        print_endline (scaro#to_string ());
        print_endline "";
        print_endline "The Doctor : i know your weakness !";
        doctor#use_sonic_screwdriver ();
        scaro#die ();
    end

let () = main ()
