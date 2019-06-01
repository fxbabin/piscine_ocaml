module Watchtower =
struct
  type hour = int
  
  let (zero: hour): hour = 0
  let add (h1: hour) (h2: hour): hour = 
    if (h1 + h2 ) mod 12 < 0 then 12 + ((h1 + h2 ) mod 12)
    else (h1 + h2 ) mod 12
  let sub (h1: hour) (h2: hour): hour = 
    if (h1 - h2 ) mod 12 < 0 then 12 + ((h1 - h2 ) mod 12)
    else (h1 - h2 ) mod 12
end