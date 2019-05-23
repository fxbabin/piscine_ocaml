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
  | A -> T
  | C -> G
  | T -> A
  | G -> C
  | None -> None

let generate_nucleotide nuc_base = 
    let nucleo_out = {
        phosphate = "phosphate";
        deoxyribose = "deoxyribose";
        nucleobase = nuc_base
    } in nucleo_out

let rec get_nth index = function
    | [] -> None
    | head :: tail -> 
        if index = 0 then head
        else get_nth (index - 1) tail


let generate_helix n =
  let x = [A; C; T; G] in 
  let rec aux n acc =
    if n <= 0 then acc
    else aux (n - 1) ((generate_nucleotide (get_nth (Random.int 4) x)) :: acc)
  in aux n []

let helix_to_string =
  let rec aux acc = function
    | [] -> ""  
    | [e] -> (print_nucleobase e.nucleobase) ^ acc
    | head :: tail -> aux ((print_nucleobase head.nucleobase) ^ acc) tail
  in aux ""

let reverse_list =
  let rec aux acc = function
    | [] -> []  
    | [e] -> e :: acc
    | head :: tail -> aux (head :: acc) tail
  in aux []

let complementary_helix =
  let rec aux acc = function
    | [] -> []  
    | [e] -> (acc @ [(generate_nucleotide (comp_nucleobase e.nucleobase))])
    | head :: tail -> aux (acc @ [(generate_nucleotide (comp_nucleobase head.nucleobase))]) tail
  in aux []

let main () =
  let xx = generate_helix 10 in
  print_endline(helix_to_string xx);
  print_endline(helix_to_string (complementary_helix xx))
      
let () = main ()