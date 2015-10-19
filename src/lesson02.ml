

let () = 
  let g = Gaussian1D.create ~mean:10. ~variance:4. in 
  Printf.printf "F(x = 8) = %f\n" @@ Gaussian1D.x g 8.;

  let g1 = Gaussian1D.create ~mean:10. ~variance:4. in 
  let g2 = Gaussian1D.create ~mean:12. ~variance:4. in 
  let g' = Gaussian1D.update g1 g2 in
  Printf.printf "u': %f, S2': %f \n" (Gaussian1D.mean g') (Gaussian1D.variance g');
  
  let g1 = Gaussian1D.create ~mean:10. ~variance:8. in 
  let g2 = Gaussian1D.create ~mean:13. ~variance:2. in 
  let g' = Gaussian1D.update g1 g2 in
  Printf.printf "u': %f, S2': %f \n" (Gaussian1D.mean g') (Gaussian1D.variance g');

  let history = [
    `Measurement 5.; 
    `Move 1.;
    `Measurement 6.; 
    `Move 1.;
    `Measurement 7.; 
    `Move 2.;
    `Measurement 9.; 
    `Move 1.;
    `Measurement 10.; 
    `Move 1.;
  ] in 
 
  let g = List.fold_left (fun g -> function 
    | `Measurement x -> Gaussian1D.update g (Gaussian1D.create ~mean:x ~variance:4.)
    | `Move x -> Gaussian1D.predict g (Gaussian1D.create ~mean:x ~variance:2.)
  ) (Gaussian1D.create ~mean:0. ~variance:10_000.) history in 
  Printf.printf "u': %f, S2': %f \n" (Gaussian1D.mean g) (Gaussian1D.variance g);
  ()


module L   = Lacaml.D 
module Mat = Lacaml.D.Mat  

let print_mat name a =  
    Format.printf "@[<2>%s =@\n@\n@[%a@]@]@\n@\n%!" name Lacaml.Io.pp_fmat a

let add_matrix x y = 
  L.gemm ~beta:1. ~c:(L.lacpy y) (Mat.identity (Mat.dim1 x)) x   
  (* very ineficient *)

let () = 
  let x = Mat.of_array [| 
    [|0.|] ; 
    [|0.|]
  |] in 
  (* Initial state (location, velocity *)

  let x_cov = Mat.of_array [| 
    [|1000. ; 0.    |]  ; 
    [|0.    ; 1000. |] 
  |] in
  (* Initial uncertainty *)

  let x_f = Mat.of_array [| 
    [|1. ; 1. |]  ; 
    [|0. ; 1. |] 
  |] in
  (* Next state function *)

  let z_f = Mat.of_array [| 
    [|1.; 0.|] ; 
  |] in 
  (* Measurement function *)
  
  let z_cov = Mat.of_array [| 
    [|1.|] ; 
  |] in 
  (* Measurement uncertainty *)

  let module Kf = Kalman_filter in 

  let kf = Kf.create ~dim:(2, 1) ~x_f ~z_f ~z_cov in  
  
  let _ = List.fold_left (fun (x, x_cov) measurement -> 
    let z    = Mat.of_array [| [| measurement |] |] in 
    let x, x_cov = Kf.add_measurement kf ~x ~x_cov ~z in 
    let x, x_cov = Kf.predict kf ~x ~x_cov in 
    Format.printf ">> After filter: \n%!";
    Format.printf "@[<2>X =@\n@\n@[%a@]@]@\n@\n" Lacaml.Io.pp_fmat x; 
    Format.printf "@[<2>X covariance =@\n@\n@[%a@]@]@\n@\n" Lacaml.Io.pp_fmat x_cov;
    (x, x_cov) 
  ) (x, x_cov) [1. ; 2.; 3.; 4. ; 4. ; 4. ;4. ; 4. ; 4. ;4. ; 4. ; 4. ;4. ; 4. ; 4. ;] in 
  ()
