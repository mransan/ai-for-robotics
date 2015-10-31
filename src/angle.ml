
let pi = 4. *. atan 1.0 

let normalize x = mod_float (x +. 2. *. pi) (2. *. pi)  

let degrees_of_radians x = x *. 180. /. pi
