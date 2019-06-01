module type MONOID =
    sig
        type element
        val zero1 : element
        val zero2 : element
        val mul : element -> element -> element
        val add : element -> element -> element
        val div : element -> element -> element
        val sub : element -> element -> element
    end

module INT : (MONOID with type element = int) =
    struct
        type element = int

        let (zero1:element) = 0
        let (zero2:element) = 1
        let mul:element -> element -> element = ( * )
        let add:element -> element -> element = ( + )
        let div:element -> element -> element = ( / )
        let sub:element -> element -> element = ( - )

    end

module FLOAT : (MONOID with type element = float) =
    struct
        type element = float

        let (zero1:element) = 0.0
        let (zero2:element) = 1.0
        let mul:element -> element -> element = ( *. )
        let add:element -> element -> element = ( +. )
        let div:element -> element -> element = ( /. )
        let sub:element -> element -> element = ( -. )

    end

module Calc =
    functor (M : MONOID) ->
        struct
            let mul = M.mul
            let add = M.add
            let div = M.div
            let sub = M.sub

            let rec power (x:M.element) (pow:int) : M.element = match pow with
                | 0 -> M.zero2
                | _ -> mul x (power x (pow - 1))

            let rec fact (x:M.element) : M.element = match x with
                | n when n <= M.zero2 -> M.zero2
                | n -> mul x (fact (sub x M.zero2))

        end


module Calc_int = Calc(INT)
module Calc_float = Calc(FLOAT)

let () = 
    print_int (Calc_int.add 21 21);
    print_endline "";
    print_int (Calc_int.sub 53 11);
    print_endline "";
    print_int (Calc_int.mul 21 2);
    print_endline "";
    print_int (Calc_int.div 168 4);
    print_endline "";
    print_float (Calc_float.add 42.0 0.42);
    print_endline "";
    print_float (Calc_float.sub 43.0 0.58);
    print_endline "";
    print_float (Calc_float.mul 21.21 2.0);
    print_endline "";
    print_float (Calc_float.div 169.68 4.0);
    print_endline "";
    print_float (Calc_float.power 2.5 3);
    print_endline "";
    print_int (Calc_int.power 10 3);
    print_endline "";
    print_int (Calc_int.fact 5);
    print_endline "";
    print_float (Calc_float.fact 5.0);
    print_endline "";
    print_float (Calc_float.fact 5.5);
    print_endline "";
    print_endline "end of custom tests";
    print_endline (string_of_int (Calc_int.power 3 3));
    print_endline (string_of_float (Calc_float.power 3.0 3));
    print_endline (string_of_int (Calc_int.mul (Calc_int.add 20 1) 2));
    print_endline (string_of_float (Calc_float.mul (Calc_float.add 20.0 1.0) 2.0))
