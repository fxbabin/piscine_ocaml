
module StringHash = 
struct
    type t = string

    let equal (s1: t) (s2: t) =
      s1 = s2
    
    let hash str =
      let s_len = String.length str in
      let rec aux idx hash = match idx with
        | idx when idx >= s_len -> hash
        | _ ->
          aux (idx + 1) (hash * 33 + int_of_char(String.get str idx)) 
      in aux 0 5381
end

module StringHashtbl = Hashtbl.Make (StringHash)

let () =
  let ht = StringHashtbl.create 5 in
  let values = [ "Hello"; "world"; "42"; "Ocaml"; "H" ] in
  let pairs = List.map (fun s -> (s, String.length s)) values in
  List.iter (fun (k,v) -> StringHashtbl.add ht k v) pairs;
  StringHashtbl.iter (fun k v -> Printf.printf "k = \"%s\", v = %d\n" k v) ht