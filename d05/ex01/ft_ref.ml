type 'a ft_ref = {
  mutable contents : 'a
}

let return (x: 'a) : 'a ft_ref =
  {contents=x}

let get (x:'a ft_ref) : 'a =
  x.contents

let set (x:'a ft_ref) y : unit =
  x.contents <- y

let bind (x:'a ft_ref) (f: 'a -> 'b ft_ref) : 'b ft_ref =
  f x.contents

let () =
  let x = 42 in
  let ref_x = return x in
  print_int(x);
  print_endline("");
  print_int(get ref_x);
  print_endline("");
  set ref_x 24;
  print_int(x);
  print_endline("");
  print_int(get ref_x);
  print_endline("");
  print_int(get (bind ref_x (fun x -> {contents = x + 1})));
  print_endline("")
;;