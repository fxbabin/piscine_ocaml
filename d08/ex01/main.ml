let () =
  let test = new Molecule.water in
  print_endline (test#name ^ " " ^ test#formula);
  let test = new Molecule.carbon_dioxyde in
  print_endline (test#name ^ " " ^ test#formula);
  let test = new Molecule.dihydrogen in
  print_endline (test#name ^ " " ^ test#formula);
  let test = new Molecule.cholesterol in
  print_endline (test#name ^ " " ^ test#formula);
  let test = new Molecule.stearic_acid in
  print_endline (test#name ^ " " ^ test#formula);
  ()