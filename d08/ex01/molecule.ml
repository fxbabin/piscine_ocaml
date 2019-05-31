let hill_sort atm1 atm2 = match atm1#symbol with
    | _ when (atm1#symbol = atm2#symbol) -> 0
    | "C" -> -1
    | "H" when atm2#symbol = "C" -> 1
    | "H" -> -1
    | _ when (atm1#symbol > atm2#symbol) -> 1
    | _ -> -1

let encode_to_string lst =
  let rec aux lst acc = match lst with
      | [] -> acc
      | (a, b)::tail when a = 1 -> aux tail (b ^ acc )
      | (a, b)::tail -> aux tail (b ^ (string_of_int a) ^ acc )
  in aux lst ""

let encode =
  let rec encode_aux nb acc = function
    | [] -> []
    | [e] -> acc @ [(nb + 1, e)]
    | prev :: (next :: rest as tail) ->
        if prev = next then encode_aux (nb + 1) acc tail
        else encode_aux 0 (acc @ [(nb + 1, prev)]) tail
  in encode_aux 0 []

let to_symbol_list lst = 
  let rec loop lst acc = match lst with
      | [] -> acc
      | head::tail -> loop tail (head#symbol::acc)
  in loop lst []

class virtual molecule name (atom_list:Atom.atom list) =
object (this)
  method name : string = name
  method formula : string = this#get_formula atom_list

  method get_formula lst = encode_to_string (encode (to_symbol_list (List.sort hill_sort lst)))
  method to_string = this#name ^ " " ^ this#formula
  method equals (other:molecule) = 
    if this#formula = other#formula then true
    else false
end

class water =
object
  inherit molecule "Water" [(new Atom.hydrogen); (new Atom.hydrogen); (new Atom.oxygen)]
end

class carbon_dioxyde =
object
  inherit molecule "Carbon dioxyde" [(new Atom.oxygen); (new Atom.oxygen); (new Atom.carbon)]
end

class dihydrogen =
object
  inherit molecule "dihydrogen" [(new Atom.hydrogen); (new Atom.hydrogen)]
end

class acetylsalicylic_acid = 
object 
    inherit molecule "acetylsalicylic acid" [(new Atom.oxygen); (new Atom.oxygen); (new Atom.oxygen); (new Atom.oxygen); (new Atom.carbon); (new Atom.carbon); (new Atom.carbon); (new Atom.carbon); (new Atom.carbon); (new Atom.carbon); (new Atom.carbon); (new Atom.carbon); (new Atom.carbon); (new Atom.hydrogen); (new Atom.hydrogen); (new Atom.hydrogen); (new Atom.hydrogen); (new Atom.hydrogen); (new Atom.hydrogen); (new Atom.hydrogen); (new Atom.hydrogen);]
end

class cholesterol = 
object 
    inherit molecule "cholesterol" [(new Atom.oxygen); (new Atom.carbon); (new Atom.carbon); (new Atom.carbon); (new Atom.carbon); (new Atom.carbon); (new Atom.carbon); (new Atom.carbon); (new Atom.carbon); (new Atom.carbon); (new Atom.carbon); (new Atom.carbon); (new Atom.carbon); (new Atom.carbon); (new Atom.carbon); (new Atom.carbon); (new Atom.carbon); (new Atom.carbon); (new Atom.carbon); (new Atom.carbon); (new Atom.carbon); (new Atom.carbon); (new Atom.carbon); (new Atom.carbon); (new Atom.carbon); (new Atom.carbon); (new Atom.carbon); (new Atom.carbon); (new Atom.hydrogen); (new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen)]
end

class stearic_acid =
object
  inherit molecule "Stearic acid" [(new Atom.oxygen);(new Atom.oxygen);(new Atom.carbon);(new Atom.carbon);(new Atom.carbon);(new Atom.carbon);(new Atom.carbon);(new Atom.carbon);(new Atom.carbon);(new Atom.carbon);(new Atom.carbon);(new Atom.carbon);(new Atom.carbon);(new Atom.carbon);(new Atom.carbon);(new Atom.carbon);(new Atom.carbon);(new Atom.carbon);(new Atom.carbon);(new Atom.carbon);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen)]
end

class lysin =
object
  inherit molecule "Lysin" [(new Atom.oxygen);(new Atom.oxygen);(new Atom.nitrogen);(new Atom.nitrogen);(new Atom.carbon);(new Atom.carbon);(new Atom.carbon);(new Atom.carbon);(new Atom.carbon);(new Atom.carbon);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen);(new Atom.hydrogen)]
end
