type phosphate = string
type deoxyribose = string
type nucleobase = A | T | U | C | G | None
type nucleotide = {
    phosphate: phosphate;
    deoxyribose: deoxyribose;
    nucleobase: nucleobase
}

type helix = nucleotide list
type rna = nucleobase list

let print_nucleobase nuc = match nuc with 
  | A -> "A"
  | C -> "C"
  | T -> "T"
  | G -> "G"
  | U -> "U"
  | None -> "None"

let comp_nucleobase nuc = match nuc with 
  | A -> T
  | C -> G
  | T -> A
  | G -> C
  | U -> A
  | None -> None

let comp_rna_encode nuc = match nuc with 
  | A -> U
  | C -> G
  | T -> A
  | G -> C
  | U -> A
  | None -> None

let get_nucleobase nuc = match nuc with 
  | 0 -> T
  | 1 -> G
  | 2 -> A
  | 3 -> C
  | _ -> None

let generate_nucleotide nuc_base = 
    let nucleo_out = {
        phosphate = "phosphate";
        deoxyribose = "deoxyribose";
        nucleobase = nuc_base
    } in nucleo_out

let generate_helix n =
  let rec aux n acc =
    if n <= 0 then acc
    else aux (n - 1) ((generate_nucleotide (get_nucleobase (Random.int 4))) :: acc)
  in aux n []

let helix_to_string =
  let rec aux acc = function
    | [] -> ""  
    | [e] -> (print_nucleobase e.nucleobase) ^ acc
    | head :: tail -> aux ((print_nucleobase head.nucleobase) ^ acc) tail
  in aux ""

let complementary_helix =
  let rec aux acc = function
    | [] -> []  
    | [e] -> (acc @ [(generate_nucleotide (comp_nucleobase e.nucleobase))])
    | head :: tail -> aux (acc @ [(generate_nucleotide (comp_nucleobase head.nucleobase))]) tail
  in aux []

let generate_rna =
  let rec aux acc = function
    | [] -> []  
    | [e] -> (acc @ [(generate_nucleotide (comp_rna_encode e.nucleobase))])
    | head :: tail -> aux (acc @ [(generate_nucleotide (comp_rna_encode head.nucleobase))]) tail
  in aux []

let main () =
  let xx = generate_helix 10 in
  print_endline(helix_to_string xx);
  print_endline(helix_to_string (complementary_helix xx));
  print_endline(helix_to_string (generate_rna xx))

let () = main ()