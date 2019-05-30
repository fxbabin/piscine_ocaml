module Stringt =
struct
  type t = string

  let compare (s1: t) (s2: t) =
    if s1 > s2 then 1
    else if s1 < s2 then -1
    else 0
end

module StringSet = Set.Make (Stringt)

let () =
  let set = List.fold_right StringSet.add [ "foo"; "bar"; "baz"; "qux" ] StringSet.empty in
  StringSet.iter print_endline set;
  print_endline (StringSet.fold ( ^ ) set "")