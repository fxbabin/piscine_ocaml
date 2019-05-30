let main () = 
    let test = new Doctor.doctor in 
    begin
        test#talk ();
		print_endline (test#to_string ());
        test#use_sonic_screwdriver ();
        ignore (test#travel_in_time 10 20);
		print_endline (test#to_string ())
    end

let () = main ()
