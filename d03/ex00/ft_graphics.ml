let draw_square x y size =
  Graphics.moveto (x - (size / 2)) (y - (size / 2));
  Graphics.lineto ((x - (size / 2)) + size)  (y - (size / 2));
  Graphics.lineto (x - (size / 2) + size)  ((y - (size / 2)) + size);
  Graphics.moveto (x + (size / 2)) (y + (size / 2));
  Graphics.lineto ((x + (size / 2)) - size)  (y + (size / 2));
  Graphics.lineto (x + (size / 2) - size)  ((y + (size / 2)) - size)

type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let draw_square_text x y size text =
  Graphics.moveto x y;
  Graphics.draw_string text;
  draw_square x y size

let draw_tree_node = function
  | Node (v, Nil, Nil) -> (
      draw_square_text 100 100 50 v;
      draw_square_text 200 150 50 "Nil";
      draw_square_text 200 50 50 "Nil";
      Graphics.moveto 125 100;
      Graphics.lineto 175 150;
      Graphics.moveto 125 100;
      Graphics.lineto 175 50
  )
  | Nil -> draw_square_text 100 100 50 "Nil"
  | _ -> draw_square_text 100 100 50 "Nil"

let main () =
  Graphics.open_graph " 300x200";
  draw_tree_node Nil;
  ignore (read_line ());

  Graphics.open_graph " 300x200";
  draw_tree_node (Node("v", Nil, Nil));
  ignore (read_line ());
  ()

let () = main ()