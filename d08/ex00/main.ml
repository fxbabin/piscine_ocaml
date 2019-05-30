let () =
  let test = new Atom.hydrogen in
  print_endline (test#name ^ " " ^ test#symbol ^ " " ^ (string_of_int test#atomic_number));
  let test = new Atom.carbon in
  print_endline (test#name ^ " " ^ test#symbol ^ " " ^ (string_of_int test#atomic_number));
  let test = new Atom.oxygen in
  print_endline (test#name ^ " " ^ test#symbol ^ " " ^ (string_of_int test#atomic_number));
  let test = new Atom.nitrogen in
  print_endline (test#name ^ " " ^ test#symbol ^ " " ^ (string_of_int test#atomic_number));
  let test = new Atom.sulfur in
  print_endline (test#name ^ " " ^ test#symbol ^ " " ^ (string_of_int test#atomic_number));
  let test = new Atom.iron in
  print_endline (test#name ^ " " ^ test#symbol ^ " " ^ (string_of_int test#atomic_number));
  let test = new Atom.calcium in
  print_endline (test#name ^ " " ^ test#symbol ^ " " ^ (string_of_int test#atomic_number));
  let test = new Atom.potassium in
  print_endline (test#name ^ " " ^ test#symbol ^ " " ^ (string_of_int test#atomic_number));
  let test = new Atom.helium in
  print_endline (test#name ^ " " ^ test#symbol ^ " " ^ (string_of_int test#atomic_number));
  let test = new Atom.sodium in
  print_endline (test#name ^ " " ^ test#symbol ^ " " ^ (string_of_int test#atomic_number));
  let test = new Atom.magnesium in
  print_endline (test#name ^ " " ^ test#symbol ^ " " ^ (string_of_int test#atomic_number));
  let test = new Atom.arsenic in
  print_endline (test#name ^ " " ^ test#symbol ^ " " ^ (string_of_int test#atomic_number));
  ()