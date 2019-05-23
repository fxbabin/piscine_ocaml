type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | None
type nucleotide = {
    phosphate: phosphate;
    deoxyribose: deoxyribose;
    nucleobase: nucleobase
}

type helix = nucleotide list

let print_nucleobase nuc = match nuc with 
  | A -> "A"
  | C -> "C"
  | T -> "T"
  | G -> "G"
  | None -> "None"

let comp_nucleobase nuc = match nuc with 
  | A -> 'T'
  | C -> 'G'
  | T -> 'A'
  | G -> 'C'
  | None -> ' '

let get_nucleobase nuc = match nuc with 
  | 0 -> 'T'
  | 1 -> 'G'
  | 2 -> 'A'
  | 3 -> 'C'
  | _ -> ' '

  let generate_nucleotide nuc_base = match nuc_base with
  | 'A' -> { phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = A}
  | 'T' -> { phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = T}
  | 'C' -> { phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = C}
  | 'G' -> { phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = G}
  | _ -> { phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = None}

let generate_helix n : helix =
  let rec aux n acc =
    if n <= 0 then acc
    else aux (n - 1) ((generate_nucleotide (get_nucleobase (Random.int 4))) :: acc)
  in aux n []

let helix_to_string (helix: helix) =
  let rec aux acc = function
    | [] -> ""  
    | [e] ->  acc ^ (print_nucleobase e.nucleobase)
    | head :: tail -> aux (acc ^ (print_nucleobase head.nucleobase)) tail
  in aux "" helix

let complementary_helix (helix: helix) : helix =
  let rec aux acc = function
    | [] -> []  
    | [e] -> (acc @ [(generate_nucleotide (comp_nucleobase e.nucleobase))])
    | head :: tail -> aux (acc @ [(generate_nucleotide (comp_nucleobase head.nucleobase))]) tail
  in aux [] helix

let main () =
  let xx= generate_helix 10 in
  print_endline(helix_to_string xx);
  print_endline(helix_to_string (complementary_helix xx))
      
let () = main ()