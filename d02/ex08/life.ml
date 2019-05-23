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
  | A -> 'T'
  | C -> 'G'
  | T -> 'A'
  | G -> 'C'
  | U -> 'A'
  | None -> ' '

let comp_rna_encode nuc = match nuc with 
  | A -> U
  | C -> G
  | T -> A
  | G -> C
  | U -> A
  | None -> None

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

let generate_rna (helix: helix) : rna =
  let rec aux acc = function
    | [] -> []  
    | [e] -> (acc @ [(comp_rna_encode e.nucleobase)])
    | head :: tail -> aux (acc @ [(comp_rna_encode head.nucleobase)]) tail
  in aux [] helix

let rna_to_string (rna: rna) =
  let rec aux acc = function
    | [] -> ""  
    | [e] -> acc ^ (print_nucleobase e)
    | head :: tail -> aux (acc ^ (print_nucleobase head)) tail
  in aux "" rna

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

let generate_bases_triplets (rna: rna) =
  let rec aux acc = function
    | [] -> acc
    | nuc1 :: [] -> acc
    | nuc1 :: nuc2 :: [] -> acc
    | nuc1 :: nuc2 :: nuc3 :: tail -> aux (acc @ [(nuc1, nuc2, nuc3)]) tail
  in aux [] rna

let print_triplets = 
  let rec aux acc = function
    | [] -> acc
    | (a, b, c) :: tail -> aux (acc ^ ("(" ^ (print_nucleobase a) ^ "," ^ (print_nucleobase b) ^ "," ^ (print_nucleobase c) ^ "); ")) tail
  in aux ""

let string_of_protein (prot: protein) =
  let rec aux acc = function
    | [] -> acc 
    | head :: tail -> aux (acc ^ (string_of_aminoacid head) ^ " ") tail
  in aux "" prot

let decode_arn (rna: rna) = 
  let triplets = generate_bases_triplets rna in
  let rec aux acc = function
    | [] -> acc 
    | head :: tail ->
      if (which_aminoacid head) = Stop then acc 
      else aux (acc @ [(which_aminoacid head)]) tail
  in aux [] triplets

let life (str: string) = 
  let xx = generate_helix 10 in
  print_endline(helix_to_string xx);
  print_endline(rna_to_string (generate_rna xx));
  print_endline(print_triplets (generate_bases_triplets (generate_rna xx)));
  print_endline(string_of_protein (decode_arn (generate_rna xx)))

let main () =
  life ("")   

let () = main ()