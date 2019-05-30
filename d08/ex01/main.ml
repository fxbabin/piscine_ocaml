let () =
  let test = new Molecule.water in
  print_endline (test#name ^ " " ^ test#formula);
  let test = new Molecule.carbon_dioxyde in
  print_endline (test#name ^ " " ^ test#formula);
  let test = new Molecule.chlorophyll in
  print_endline (test#name ^ " " ^ test#formula);
  let test = new Molecule.adenosine_triphosphate in
  print_endline (test#name ^ " " ^ test#formula);
  let test = new Molecule.oleanolic_acid in
  print_endline (test#name ^ " " ^ test#formula);
  let test = new Molecule.tetrodotoxin in
  print_endline (test#name ^ " " ^ test#formula);
  let test = new Molecule.aspirin in
  print_endline (test#name ^ " " ^ test#formula);
  ()