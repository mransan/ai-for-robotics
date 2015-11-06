module V2D = Velocity2D
module Mat = Matrix_util 
module Vec = Vector_util

open Ekf

let h = Mat.identity 3 

let mat_of_pos {Pos2D.x;y;theta} = Mat.of_array [| 
  [| x |];
  [| y |];
  [| theta |];
|]

let pos_of_mat m = 
  let a = Mat.to_array m in 
  {Pos2D.x = a.(0).(0); y = a.(1).(0); theta = a.(2).(0); } 

module X = struct 
  type x = Pos2D.t 
  let mat_of_x = mat_of_pos 
  let x_of_mat = pos_of_mat 
end 

module U = struct 
  type x = Pos2D.t 
  type u = (Velocity2D.t * Velocity2D.dt * Velocity2D.noise) 

  let g pos (v, dt, n) =
    Velocity2D.update_pos v dt pos 

   let jacobian (v, dt, _) pos = 
     Velocity2D.pos_jacobian v dt pos 

   let r (vel, dt, noise) pos = 
     let open Mat.Ops in 
     let v = V2D.motion_jacobian vel dt pos in 
     let m = V2D.noise_matrix vel noise in 
     v *~ m *~ (Mat.transpose v) 
     
end 
(*
 

TODO: complete with new Ekf.

module Ekfp = Ekf.Make_prediction(X)(U) 

let () = 
  let s  = 
    let s = float_of_string Sys.argv.(1) in 
    Mat.of_array [|
      [|s   ; 0.  ; 0.  ;|];
      [|0.  ; 0.  ; 0.  ;|];
      [|0.  ; 0.  ; 0.  ;|];
    |] in 
  let u  = Velocity2D.create ~v:1. ~w:0. () in 
  let dt = 1. in 
  let noise = Velocity2D.create_noise 0.1 0.0 0. 0. in 
  
  let x, s = Util.fold_n (fun (x, s) -> 
    Ekfp.predict x s (u, dt, noise)  
  ) (Pos2D.zero, s) 2
  in 
  Printf.printf "Pos: %s\n" @@ Pos2D.to_string x; 
  Mat.print "S" s; 
  ()



let kf_update p s z q = 
  let z  = mat_of_pos z in 
  let zp = mat_of_pos p in 

  let open Mat.Ops in 
  let si = h *~ s *~ (Mat.transpose h) +~ q in 
  Lacaml_D.getri si;
  let ki = s *~ (Mat.transpose h) *~ si in 
  let p  = mat_of_pos p  +~ ki *~ (z -~ zp) in  
  let s  = (Mat.identity 3 -~ ki *~h) *~ s in 
  (pos_of_mat p, s)




let () = 
  
  let p  = Pos2D.zero in 
  
  let ap = Pos2D.create ~x:1. ~y:1. ~theta:0. in 
  let s  = 
    let s = float_of_string Sys.argv.(1) in 
    Mat.of_array [|
      [|s   ; 0.  ; 0.  ;|];
      [|0.  ; s   ; 0.  ;|];
      [|0.  ; 0.  ; s   ;|];
    |] in 

  let q  = 
    let q = float_of_string Sys.argv.(2) in 
    Mat.of_array [|
      [| q  ; 0.  ; 0.  |]; 
      [| 0. ; q   ; 0.  |]; 
      [| 0. ; 0.  ; q /. 2.   |]; 
    |]
  in 

  print_endline "Before Kalman Update";
  Mat.print "Belief pos" (mat_of_pos p); 
  Mat.print "Actual pos" (mat_of_pos ap); 
  Mat.print "Covariance pos" s; 
  
  print_endline "Measurement:";

  let (p, s) = kf_update p s ap q in 

  print_endline "After Kalman Update";
  Mat.print "Belief pos" (mat_of_pos p); 
  Mat.print "Covariance pos" s; 
  
  let (values, vectors) = Mat.eigen_decomposition ~n:3 s in 
  Vec.print "Eigen values" values;
  Mat.print "Eigen vectors" vectors;
  ()
*)
