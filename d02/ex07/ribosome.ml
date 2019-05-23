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

type aminoacid = Stop | Ala | Arg | Asn | Asp | Cys | Gln | Glu | Gly | His | Ile | Leu | Lys | Met | Phe | Pro | Ser | Thr | Trp | Tyr | Val
type protein = aminoacid list

let string_of_aminoacid amino =
  match amino with
  | Stop -> "End of translation"
  | Ala -> "Alanine"
  | Arg -> "Arginine"
  | Asn -> "Asparagine"
  | Asp -> "Aspartique"
  | Cys -> "Cysteine"
  | Gln -> "Glutamine"
  | Glu -> "Glutamique"
  | Gly -> "Glycine"
  | His -> "Histidine"
  | Ile -> "Isoleucine"
  | Leu -> "Leucine"
  | Lys -> "Lysine"
  | Met -> "Methionine"
  | Phe -> "Phenylalanine"
  | Pro -> "Proline"
  | Ser -> "Serine"
  | Thr -> "Threonine"
  | Trp -> "Tryptophane"
  | Tyr -> "Tyrosine"
  | Val -> "Valine"

let which_aminoacid triplet =
  match triplet with
  | (U, A, A) | (U, A, G) | (U, G, A) -> Stop
  | (G, C, A) | (G, C, C) | (G, C, G) | (G, C, U) -> Ala
  | (A, G, A) | (A, G, G) | (C, G, A) | (C, G, C) | (C, G, G) | (C, G, U) -> Arg
  | (A, A, C) | (A, A, U) -> Asn
  | (G, A, C) | (G, A, U) -> Asp
  | (U, G, C) | (U, G, U) -> Cys
  | (C, A, A) | (C, A, G) -> Gln
  | (G, A, A) | (G, A, G) -> Glu
  | (G, G, A) | (G, G, C) | (G, G, G) | (G, G, U) -> Gly
  | (C, A, C) | (C, A, U) -> His
  | (A, U, A) | (A, U, C) | (A, U, U) -> Ile
  | (C, U, A) | (C, U, C) | (C, U, G) | (C, U, U) | (U, U, A) | (U, U, G) -> Leu
  | (A, A, A) | (A, A, G) -> Lys
  | (A, U, G) -> Met
  | (U, U, C) | (U, U, U) -> Phe
  | (C, C, C) | (C, C, A) | (C, C, G) | (C, C, U) -> Pro
  | (U, C, A) | (U, C, C) | (U, C, G) | (U, C, U) | (A, G, U) | (A, G, C) -> Ser
  | (A, C, A) | (A, C, C) | (A, C, G) | (A, C, U) -> Thr
  | (U, G, G) -> Trp
  | (U, A, C) | (U, A, U) -> Tyr
  | (G, U, A) | (G, U, C) | (G, U, G) | (G, U, U) -> Val
  | _ -> Stop

let main () =
  let xx = generate_helix 10 in
  print_endline(helix_to_string xx);
  print_endline(helix_to_string (complementary_helix xx));
  print_endline(helix_to_string (generate_rna xx))

let () = main ()