class virtual molecule n f =
object (this)
  method name : string = n
  method formula : string = f

  method to_string = this#name ^ " " ^ this#formula
end

class water =
object
  inherit molecule "Water" "H2O"
end

class carbon_dioxyde =
object
  inherit molecule "Carbon dioxyde" "CO2"
end

class chlorophyll =
object
  inherit molecule "Chlorophyll" "C55H72O5N4Mg"
end

class adenosine_triphosphate = 
object
  inherit molecule "Adenosine triphosphate" "C10H16N5O13P3"
end

class oleanolic_acid =
object
  inherit molecule "Oleanolic acid" "C30H48O3"
end

class tetrodotoxin =
object
  inherit molecule "Tetrodotoxin" "C11H17N3O8"
end

class aspirin =
object
  inherit molecule "Aspirin" "C9H8O4"
end