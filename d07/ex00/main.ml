let main () = 
    let test = new People.people in 
    begin
        print_endline (test#to_string ());
        test#announce ();
        test#die ()
    end

let () = main ()
