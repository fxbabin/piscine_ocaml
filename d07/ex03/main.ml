let main () = 
    let scaro = new Dalek.dalek in 
    let doctor = new Doctor.doctor in
    let amy = doctor#get_sidekick () in
    let army_dalek = new Army.army in
    let army_amy = new Army.army in
    let army_doctor = new Army.army in
    begin
        print_endline "";
        army_dalek#add scaro;
        army_dalek#add (new Dalek.dalek);
        army_dalek#add scaro;
        army_dalek#delete ();
        army_dalek#print ();

        army_amy#add amy;
        army_amy#add amy;
        army_amy#add amy;
        army_amy#delete ();
        army_amy#print ();

        army_doctor#add doctor;
        army_doctor#add doctor;
        army_doctor#add doctor;
        army_doctor#delete ();
        army_doctor#print ();
    end

let () = main ()
