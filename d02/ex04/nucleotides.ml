type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | None
type nucleotide = {
    phosphate: phosphate;
    deoxyribose: deoxyribose;
    nucleobase: nucleobase
}

let print_nucleobase nuc = match nuc with 
    | A -> "A"
    | C -> "C"
    | T -> "T"
    | G -> "G"
    | None -> "None"

let generate_nucleotide nuc_base = match nuc_base with
    | 'A' -> { phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = A}
    | 'T' -> { phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = T}
    | 'C' -> { phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = C}
    | 'G' -> { phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = G}
    | _ -> { phosphate = "phosphate"; deoxyribose = "deoxyribose"; nucleobase = None}

let main () =
    let nuc_out = generate_nucleotide 'A'
    in
    print_endline(nuc_out.phosphate);
    print_endline(nuc_out.deoxyribose);
    print_endline(print_nucleobase nuc_out.nucleobase)
    
let () = main ()