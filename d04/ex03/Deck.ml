module Color =
struct
    type t = Spade | Heart | Diamond | Club

    let all = [Spade; Heart ; Diamond ; Club]

    let toString card = match card with
        | Spade -> "S"
        | Heart -> "H"
        | Diamond -> "D"
        | Club -> "C"
    
    let toStringVerbose card = match card with
        | Spade -> "Spade"
        | Heart -> "Heart"
        | Diamond -> "Diamond"
        | Club -> "Club"

end

module Value =
struct
    type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As
    
    let all = [T2 ; T3 ; T4 ; T5 ; T6 ; T7 ; T8 ; T9 ; T10 ; Jack ; Queen ; King ; As]
    
    let toInt value = match value with
        | T2 -> 1
        | T3 -> 2
        | T4 -> 3
        | T5 -> 4
        | T6 -> 5
        | T7 -> 6
        | T8 -> 7
        | T9 -> 8
        | T10 -> 9
        | Jack -> 10
        | Queen -> 11
        | King -> 12
        | As -> 13
    
    let toString value = match value with
        | T2 -> "2"
        | T3 -> "3"
        | T4 -> "4"
        | T5 -> "5"
        | T6 -> "6"
        | T7 -> "7"
        | T8 -> "8"
        | T9 -> "9"
        | T10 -> "10"
        | Jack -> "J"
        | Queen -> "Q"
        | King -> "K"
        | As -> "A"
    
    let toStringVerbose value = match value with
        | T2 -> "2"
        | T3 -> "3"
        | T4 -> "4"
        | T5 -> "5"
        | T6 -> "6"
        | T7 -> "7"
        | T8 -> "8"
        | T9 -> "9"
        | T10 -> "10"
        | Jack -> "Jack"
        | Queen -> "Queen"
        | King -> "King"
        | As -> "As"
    
    let next value = match value with
        | T2 -> T3
        | T3 -> T4
        | T4 -> T5
        | T5 -> T6
        | T6 -> T7
        | T7 -> T8
        | T8 -> T9
        | T9 -> T10
        | T10 -> Jack
        | Jack -> Queen
        | Queen -> King
        | King -> As
        | As -> invalid_arg "As has no next"
    
    let previous value = match value with
        | T2 -> invalid_arg "2 has no previous"
        | T3 -> T2
        | T4 -> T3
        | T5 -> T4
        | T6 -> T5
        | T7 -> T6
        | T8 -> T7
        | T9 -> T8
        | T10 -> T8
        | Jack -> T10
        | Queen -> Jack
        | King -> Queen
        | As -> King

end

module Card = 
struct
    type t = {
        value : Value.t;
        color : Color.t
    }
    
    let newCard value color = {value = value; color = color}
    let allSpades = [{value=T2;color=Spade} ; {value=T3;color=Spade}; {value=T4;color=Spade}; {value=T5;color=Spade}; {value=T6;color=Spade}; {value=T7;color=Spade}; {value=T8;color=Spade}; {value=T9;color=Spade}; {value=T10;color=Spade}; {value=Jack;color=Spade}; {value=Queen;color=Spade}; {value=King;color=Spade}; {value=As;color=Spade}]
    let allHearts = [{value=T2;color=Heart} ; {value=T3;color=Heart}; {value=T4;color=Heart}; {value=T5;color=Heart}; {value=T6;color=Heart}; {value=T7;color=Heart}; {value=T8;color=Heart}; {value=T9;color=Heart}; {value=T10;color=Heart}; {value=Jack;color=Heart}; {value=Queen;color=Heart}; {value=King;color=Heart}; {value=As;color=Heart}]
    let allDiamonds = [{value=T2;color=Diamond} ; {value=T3;color=Diamond}; {value=T4;color=Diamond}; {value=T5;color=Diamond}; {value=T6;color=Diamond}; {value=T7;color=Diamond}; {value=T8;color=Diamond}; {value=T9;color=Diamond}; {value=T10;color=Diamond}; {value=Jack;color=Diamond}; {value=Queen;color=Diamond}; {value=King;color=Diamond}; {value=As;color=Diamond}]
    let allClubs = [{value=T2;color=Club} ; {value=T3;color=Club}; {value=T4;color=Club}; {value=T5;color=Club}; {value=T6;color=Club}; {value=T7;color=Club}; {value=T8;color=Club}; {value=T9;color=Club}; {value=T10;color=Club}; {value=Jack;color=Club}; {value=Queen;color=Club}; {value=King;color=Club}; {value=As;color=Club}]
    let all = allSpades@allHearts@allDiamonds@allClubs
    
    
    let getValue card = card.value
    let getColor card = card.color
    
    
    let toString card = (Value.toString (getValue card))^(Color.toString (getColor card))
    let toStringVerbose card = "Card("^(Value.toStringVerbose (getValue card))^","^(Color.toStringVerbose (getColor card))^")"
    
    
    let compare card1 card2 = (Value.toInt card1.value) - (Value.toInt card2.value)
    let max card1 card2 = match (Value.toInt card1.value) with
        | n when n >= (Value.toInt card2.value) -> card1
        | _ -> card2
    
    let min card1 card2 = match (Value.toInt card1.value) with
        | n when n <= (Value.toInt card2.value) -> card1
        | _ -> card2
    
    let best lst = 
        if lst = [] then invalid_arg "string must not be empty"
        else
            let rec loop lst save = match lst with
                | [] -> save
                | head::tail -> loop tail (max save head)
            in loop lst (List.hd lst)
    
    let isOf card color = match card.color with
        | c when c = color -> true
        | _ -> false
    
    let isSpade card = (isOf card Spade)
    let isHeart card = (isOf card Heart)
    let isDiamond card = (isOf card Diamond)
    let isClub card = (isOf card Club)
end


