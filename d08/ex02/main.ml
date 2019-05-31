let () =
  let test = new Alkane.methane in
  print_endline (test#name ^ " " ^ test#formula);
  let test = new Alkane.ethane in
  print_endline (test#name ^ " " ^ test#formula);
  let test = new Alkane.propane in
  print_endline (test#name ^ " " ^ test#formula);
  let test = new Alkane.butane in
  print_endline (test#name ^ " " ^ test#formula);
  let test = new Alkane.pentane in
  print_endline (test#name ^ " " ^ test#formula);
  let test = new Alkane.hexane in
  print_endline (test#name ^ " " ^ test#formula);
  let test = new Alkane.heptane in
  print_endline (test#name ^ " " ^ test#formula);
  let test = new Alkane.octane in
  print_endline (test#name ^ " " ^ test#formula);
  let test = new Alkane.nonane in
  print_endline (test#name ^ " " ^ test#formula);
  let test = new Alkane.decane in
  print_endline (test#name ^ " " ^ test#formula);
  let test = new Alkane.undecane in
  print_endline (test#name ^ " " ^ test#formula);
  let test = new Alkane.dodecane in
  print_endline (test#name ^ " " ^ test#formula);
  ()