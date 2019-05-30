module type FIXED = sig
  type t
  val of_float : float -> t                                                                         
  val of_int : int -> t
  val to_float : t -> float                                                                         
  val to_int : t -> int
  val to_string : t -> string
  val zero : t
  val one : t 
  val succ : t -> t                                                                                 
  val pred : t -> t                                                                                 
  val min : t -> t -> t
  val max : t -> t -> t 
  val gth : t -> t -> bool                                                                          
  val lth : t -> t -> bool
  val gte : t -> t -> bool                                                                          
  val lte : t -> t -> bool                                                                          
  val eqp : t -> t -> bool (** physical equality *) 
  val eqs : t -> t -> bool (** structural equality *)                                               
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val foreach : t -> t -> (t -> unit) -> unit
end

module type FRACTIONNAL_BITS = sig 
  val bits : int
end

module type MAKE = functor ( Var : FRACTIONNAL_BITS ) -> FIXED

module Make : MAKE = 
  functor ( Var : FRACTIONNAL_BITS ) -> 
    struct
        type t = int

        let of_float (f:float) : t = int_of_float (f *. (2. ** (float_of_int Var.bits)))
        let of_int (i:int) : t = i lsl Var.bits

        let to_float (var:t) : float = (float_of_int var) /. (2. ** (float_of_int Var.bits))
        let to_int (var:t) : int = var lsr Var.bits
        let to_string (var:t) : string = string_of_float (to_float var)

        let zero = of_int 0
        let one = of_int 1

        let succ var = (var + (1 lsl Var.bits))
        let pred var = (var - (1 lsr Var.bits))

        let min var1 var2 = if var1 > var2 then var2 else var1
        let max var1 var2 = if var1 >= var2 then var1 else var2

        let gth var1 var2 = (var1 > var2)
        let lth var1 var2 = (var1 < var2)
        let gte var1 var2 = (var1 >= var2)
        let lte var1 var2 = (var1 <= var2)
        let eqp var1 var2 = (var1 == var2)
        let eqs var1 var2 = (var1 = var2)

        let add var1 var2 = var1 + var2
        let sub var1 var2 = var1 - var2
        let mul var1 var2 = of_float (to_float var1 *. to_float var2)
        let div var1 var2 = of_float (to_float var1 /. to_float var2)

        let foreach (start:t) (stop:t) (func:(t -> unit)) : unit =
          let rec loop i =
            if i > stop then ()
            else begin (func i); loop (i + 1) end
        in loop start
    end

module Fixed4 : FIXED = Make (struct let bits = 4 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)
let () =
  let x8 = Fixed8.of_float 21.10 in
  let y8 = Fixed8.of_float 21.32 in
  let r8 = Fixed8.add x8 y8 in
  print_endline (Fixed8.to_string r8);
  Fixed4.foreach (Fixed4.zero) (Fixed4.one) (fun f -> print_endline (Fixed4.to_string f));
  let a = Fixed8.of_int (-42) in
  print_endline (Fixed8.to_string a);
  (* of_int, to_int *)
  print_char '\n';
  print_string "of_int 42: ";
  let a = Fixed8.of_int 42 in
  print_int (Fixed8.to_int a);
  print_char '\n';
  print_char '\n';

  (* zero, one *)
  print_string "zero: ";
  print_endline (Fixed8.to_string Fixed8.zero);
  print_string "one: ";
  print_endline (Fixed8.to_string Fixed8.one);
  print_char '\n';

  (* succ, pred *)
  print_string "succ 1: ";
  print_endline (Fixed8.to_string (Fixed8.succ Fixed8.one));
  print_string "pred 1: ";
  print_endline (Fixed8.to_string (Fixed8.pred Fixed8.one));
  print_char '\n';

  (* min, max *)
  print_string "min 0 1: ";
  print_endline (Fixed8.to_string (Fixed8.min Fixed8.zero Fixed8.one));
  print_string "max 0 1: ";
  print_endline (Fixed8.to_string (Fixed8.max Fixed8.zero Fixed8.one));
  print_char '\n';

  (* gth, lth, gte, lte, eqp, eqs *)
  print_string "gth 0 1: ";
  print_endline (string_of_bool (Fixed8.gth Fixed8.zero Fixed8.one));
  print_string "gth 1 0: ";
  print_endline (string_of_bool (Fixed8.gth Fixed8.one Fixed8.zero));
  print_char '\n';

  print_string "lth 0 1: ";
  print_endline (string_of_bool (Fixed8.lth Fixed8.zero Fixed8.one));
  print_string "lth 1 0: ";
  print_endline (string_of_bool (Fixed8.lth Fixed8.one Fixed8.zero));
  print_char '\n';

  print_string "gte 0 0: ";
  print_endline (string_of_bool (Fixed8.gte Fixed8.zero Fixed8.zero));
  print_string "gte 0 1: ";
  print_endline (string_of_bool (Fixed8.gte Fixed8.zero Fixed8.one));
  print_string "gte 1 0: ";
  print_endline (string_of_bool (Fixed8.gte Fixed8.one Fixed8.zero));
  print_char '\n';

  print_string "lte 0 0: ";
  print_endline (string_of_bool (Fixed8.lte Fixed8.zero Fixed8.one));
  print_string "lte 0 1: ";
  print_endline (string_of_bool (Fixed8.lte Fixed8.zero Fixed8.one));
  print_string "lte 1 0: ";
  print_endline (string_of_bool (Fixed8.lte Fixed8.one Fixed8.zero));
  print_char '\n';

  print_string "eqp 0 0: ";
  print_endline (string_of_bool (Fixed8.eqp Fixed8.zero Fixed8.zero));
  print_string "eqp 0 1: ";
  print_endline (string_of_bool (Fixed8.eqp Fixed8.zero Fixed8.one));
  print_char '\n';

  print_string "eqs 0 0: ";
  print_endline (string_of_bool (Fixed8.eqs Fixed8.zero Fixed8.zero));
  print_string "eqs 0 1: ";
  print_endline (string_of_bool (Fixed8.eqs Fixed8.zero Fixed8.one));
  print_char '\n';

  (* add, sub, mul, div *)
  let a = Fixed8.of_int 42 in
  let b = Fixed8.of_int 2 in
  print_string "add 42 2: ";
  print_endline (Fixed8.to_string (Fixed8.add a b));
  print_string "sub 42 2: ";
  print_endline (Fixed8.to_string (Fixed8.sub a b));
  print_string "mul 42 2: ";
  print_endline (Fixed8.to_string (Fixed8.mul a b));
  print_string "div 42 2: ";
  print_endline (Fixed8.to_string (Fixed8.div a b));