


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

let () = 
  print_endline "- test 1";
  let samples = [0. ; 1.] in 
  let h       = Gaussian1D.histogram_from_samples 10 samples in 
  List.iter (fun (x, hv) -> 
    Printf.printf "x: %10.5f , hv: %10.5f \n" x hv
  ) h;
  ()

let () = 
  print_endline "- test 2";
  let samples = [0. ; ] in 
  let h       = Gaussian1D.histogram_from_samples 10 samples in 
  List.iter (fun (x, hv) -> 
    Printf.printf "x: %10.5f , hv: %10.5f \n" x hv
  ) h;
  ()

let () = 
  print_endline "- test 3";
  let samples = [-1. ; 1.] in 
  let h       = Gaussian1D.histogram_from_samples 2 samples in 
  List.iter (fun (x, hv) -> 
    Printf.printf "x: %10.5f , hv: %10.5f \n" x hv
  ) h;
  ()

let () = 
  print_endline "- test 4";
  let g = Gaussian1D.create ~mean:0. ~variance:1. in 
  let samples = Util.fold_n (fun l  -> 
    (Gaussian1D.random g)::l
  ) [] 100 in 

  let h       = Gaussian1D.histogram_from_samples 10 samples in 
  List.iter (fun (x, hv) -> 
    Printf.printf "x: %10.5f , hv: %10.5f \n" x hv
  ) h;
  ()
