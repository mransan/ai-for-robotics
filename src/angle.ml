
let pi = 4. *. atan 1.0 

let normalize ?type_:(type_ = `Zero_2_pi) x = 
  match type_ with 
  | `Zero_2_pi -> mod_float (x +. 2. *. pi) (2. *. pi)  
  | `Zero_centered -> (mod_float (x +. 2. *. pi +. pi) (2. *. pi)) -. pi  

let degrees_of_radians x = x *. 180. /. pi
