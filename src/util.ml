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

let fold_righti f a x =
  let r = ref x in
  for i = Array.length a - 1 downto 0 do
    r := f i (Array.unsafe_get a i) !r
  done;
  !r

let if_some x f = 
  match x with 
  | Some x' -> f x' 
  | None    -> () 
