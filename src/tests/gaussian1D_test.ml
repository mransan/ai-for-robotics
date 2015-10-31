


let () = 

  let out = open_out "gaussian1D.csv" in 
  let g = Gaussian1D.create ~mean:0. ~variance:0.1 in 
  let rec loop x = function
    | 0 -> () 
    | n -> (
      Printf.fprintf out "%f, %f\n" x (Gaussian1D.x g x); 
      loop (x +. 0.2) (n - 1) 
    ) 
  in 
  loop (-. 5.) 100; 
  let g = Gaussian1D.create ~mean:0. ~variance:0.05 in  

  print_endline @@ string_of_float @@ Gaussian1D.x g 0.16
