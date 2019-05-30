class virtual atom n s num =
object (this)
  method name : string = n
  method symbol : string = s
  method atomic_number : int = num

  method to_string = this#name ^ " " ^ this#symbol ^ " " ^ (string_of_int this#atomic_number)
  method equals (other: atom) =
    if this#name = other#name && this#symbol = other#symbol && this#atomic_number = other#atomic_number then true
    else false
end

class hydrogen =
object
  inherit atom "Hydrogen" "H" 1
end

class carbon =
object
  inherit atom "Carbon" "C" 6
end

class oxygen =
object
  inherit atom "Oxygen" "O" 8
end

class nitrogen =
object
  inherit atom "Nitrogen" "N" 7
end

class sulfur =
object
  inherit atom "Sulfur" "S" 16
end

class iron =
object
  inherit atom "Iron" "Fe" 26
end

class calcium =
object
  inherit atom "Calcium" "Ca" 20
end

class potassium =
object
  inherit atom "Potassium" "K" 19
end

class helium =
object
  inherit atom "Helium" "He" 2
end

class sodium =
object
  inherit atom "Sodium" "Na" 11
end

class magnesium =
object
  inherit atom "Magnesium" "Mg" 12
end

class arsenic =
object
  inherit atom "Arsenic" "As" 33
end