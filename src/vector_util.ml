
module L   = Lacaml.D 
module Vec = Lacaml.D.Vec


type vec = L.vec  

let to_array = Vec.to_array 

let print name a = 
  Format.printf "@[<2>%s =@\n@\n@[%a@]@]@\n@\n%!" name Lacaml.Io.pp_fvec a

