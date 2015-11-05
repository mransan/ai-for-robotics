(** 1-D robot with velocity model *)

(** In this example we focus on the most simple 1-D 
    robot, which has a translational velocity for 
    command. 
  *) 

module Mat = Matrix_util

let mat_of_float x = Mat.of_array [| [| x |] |] 

let float_of_mat m = 
  let a = Mat.to_array m in 
  a.(0).(0) 

module X = struct 
  type x = float  

  let mat_of_x = mat_of_float
  let x_of_mat = float_of_mat 
end 

module U = struct 

  type x = X.x

  type u = (float  * float * float) 
  (** speed + dt + noise *)

  let g x (v, dt, _ ) = x +. v *. dt  

  let jacobian _ _ = Mat.of_array [| [| 1. |] |]

  let r (v, dt, noise ) _ = 
    Mat.of_array [| [| v *. dt *. noise |] |] 

end 

module Z = struct 
  type x = X.x 

  type z = (float * float)
  (** (landmark position , actual measurement) *)
 
  let h (l, m) x = 
    (l, l -. x)  

  let mat_of_z (l, m) = mat_of_float m

  let jacobian _ _ = Mat.of_array [|[| -. 1. |]|]

  let q _ _ = Mat.of_array [| [| 1. |] |]  
end 

module E = Ekf.Make(X)(U)(Z) 

let () = 
  let x = 0. in 
  let s = Mat.of_array [| [| 0.1 |] |] in
  let u = (1., 1., 0.1) in 
  let (x, s) = E.predict x s u in 
  assert (Util.within_n_eps 1. x); 
  assert (Util.within_n_eps 0.2 (Mat.to_array s).(0).(0)); 
  ()

let () = 
  let x = 0. in 
  let s = Mat.of_array [| [| 1. |] |] in
  let z = (5., 4.) in 
  let x, s = E.correct x s z in 
  Printf.printf "x: %f\n" x; 
  Mat.print "S" s; 
  ()
