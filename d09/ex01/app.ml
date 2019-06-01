module App =
struct
  type project = string * string * int
  
  let (zero: project) = ("", "", 0)

  let combine ((a, b, c): project) ((d, e, f): project): project =
    if ((c + f) / 2) > 80 then (a ^ d, "succeed", (c + f) / 2)
    else (a ^ d, "failed", (c + f) / 2)
  
  let fail ((a, b, c): project): project = (a, "failed", 0)
  let success ((a, b, c): project): project = (a, "succeed", 80) 
end

let print_proj ((a,b,c): App.project) = print_endline(a ^ " -> " ^ b ^ " -> " ^ (string_of_int c))