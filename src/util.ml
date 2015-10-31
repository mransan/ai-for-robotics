let fold_n f e0 n = 
  let rec loop acc = function 
    | 0 -> acc 
    | n -> loop (f acc) (n - 1)
  in 
  loop e0 n 

let within_n_eps ?n:(n = 2) x y = 
  abs_float (x -. y) < (float_of_int n *. epsilon_float) 

let within ~delta x y = 
  abs_float (x -. y) < delta 
