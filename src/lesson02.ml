

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

let prediction ~f ~x ~u ~p = 
  let x' = (L.gemm ~beta:1. ~c:(L.lacpy u) f x)  in
  let p' = L.gemm (L.gemm f p) (Mat.transpose @@ L.lacpy f ) in
  (x', p') 

let measurement ~z ~h ~x ~p ~r ~i = 
  let y = L.gemm ~alpha:(-. 1.) h x ~beta:(1.0) ~c:(L.lacpy z) in 
  let s = L.gemm (L.gemm h p) (Mat.transpose h) ~beta:1.0 ~c:(L.lacpy r) in 
  L.getri s; 
  let k = L.gemm (L.gemm p (Mat.transpose h)) s in 
  let x'= add_matrix x (L.gemm k y) in 
  let p'= L.gemm (L.gemm ~beta:1. ~c:(L.lacpy i) ~alpha:(-. 1.) k h) p in 
  (x', p')  

let () = 
  let x = Mat.of_array [| 
    [|0.|] ; 
    [|0.|]
  |] in 
  (* Initial state (location, velocity *)

  let p = Mat.of_array [| 
    [|1000. ; 0.    |]  ; 
    [|0.    ; 1000. |] 
  |] in
  (* Initial uncertainty *)

  let f = Mat.of_array [| 
    [|1. ; 1. |]  ; 
    [|0. ; 1. |] 
  |] in
  (* Next state function *)

  let h = Mat.of_array [| 
    [|1.; 0.|] ; 
  |] in 
  (* Measurement function *)
  
  let r = Mat.of_array [| 
    [|1.|] ; 
  |] in 
  (* Measurement uncertainty *)

  let module Kf = Kalman_filter in 

  let kf = Kf.create ~dim:(2, 1) ~state_f:f ~measurement_f:h ~measurement_uncertainty:r in  
  
  let _ = List.fold_left (fun (x, p) measurement -> 
    let z    = Mat.of_array [| [| measurement |] |] in 
    let x, p = Kf.add_measurement kf ~x ~p ~z in 
    let x, p = Kf.predict kf ~x ~p in 
    Format.printf ">> After filter: \n%!";
    Format.printf "@[<2>X =@\n@\n@[%a@]@]@\n@\n" Lacaml.Io.pp_fmat x; 
    Format.printf "@[<2>P =@\n@\n@[%a@]@]@\n@\n" Lacaml.Io.pp_fmat p;
    (x, p) 
  ) (x, p) [1. ; 2.; 3.; 4. ; 4. ; 4. ;4. ; 4. ; 4. ;4. ; 4. ; 4. ;4. ; 4. ; 4. ;] in 
  ()
