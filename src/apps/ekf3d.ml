
module Mat = Matrix_util 
module Vec = Vector_util 

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
     let v = Velocity2D.motion_jacobian vel dt pos in 
     let m = Velocity2D.noise_matrix vel noise in 
     v *~ m *~ (Mat.transpose v) 
     
end 


module Z = struct 
  type x = Pos2D.t 

  type z = (Pos2D.t * Mat.mat) 
  (** (landmark, measurement *) 
  
  let mat_of_z (_, m) = m 
  
  let h (l, _) x =  
    let m = Mat.of_array [|
      [| Pos2D.distance ~from:x l |]; 
      [| Pos2D.bearing  ~from:x l |]; 
      [| 1.                       |];
    |] in
   (l, m) 

  let jacobian ({Pos2D.x = l_x; y = l_y; _}, _) {Pos2D.x ; y; theta} =
    let q = (l_x -. x)**2. +. (l_y -. y)**2. in 
    Mat.of_array [|
      [|-. (l_x -. x) /. sqrt q ; -. (l_y -. y)/.sqrt q;  0.  |];
      [|   (l_y -. y) /.      q ; -. (l_x -. x)/.     q; -1.  |];
      [|        0.              ;         0.           ;  0.  |];
    |] 

  let q _ _ = 
    let s = 0.5 in 
    Mat.of_array [| 
      [| s  ; 0.  ; 0. |]; 
      [| 0. ;  s  ; 0. |]; 
      [| 0. ; 0.  ; s  |]; 
    |] 
end 

module Ekf = Ekf.Make(X)(U)(Z) 


module Html = struct 

  (** Function which returns (rx, ry, theta) for displaying the 
      covariance matrix s to a 2d ellipse. 
      
      rx: x radius
      ry: y radius 
      theta: rotation angle 
   *)
  let of_s s = 
    let (svalues, svectors) = Mat.eigen_decomposition ~n:2 s in 
    let svalues = Vec.to_array svalues in 
    let svectors = Mat.to_array svectors in 
    let rx = svalues.(0) in 
    let ry = svalues.(1) in 
    let theta = Angle.degrees_of_radians @@ Angle.normalize ~type_:`Zero_centered @@ atan2 svectors.(1).(0) svectors.(0).(0) in 
    rx, ry, theta 
  
  (** Function to compute the application data at each time step 
   *)
  let time_i {Pos2D.x; y; theta} s = 
    let rx, ry, rotation = of_s s in 
    let data = x::y::theta::rx::ry::rotation::[] in 
    `List (List.map (fun x -> `Float x) data)  
end 

let () = 
  (** This example is looking at the [Efk.correct] method 
      only. 

      We start with a state [x] and covariance [s] and then 
      we keep correcting the state with the same set of measurement [zs]

      The goal is to demonstrate that eventually we converge
      to the true position of the robot [x']. The display 
      allows us to see how fast the convergence happens with different 
      parameters of:
      {ul 
      {- x': how far from the original belief is the true position}
      {- s : initial incertainty of the belief }
      {- q : measurement incertainty (ie even if our actual measurements are
             perfect the Kalman filter assumes they have uncertainty of [q]  }
      {- steps: how many times to we repeat the [Ekf.correct] steps. }
      }
    *)
  
  (* Initial belief [x] (state) and [s] (covariance) *) 

  let x = Pos2D.create ~x:0. ~y:0.1 ~theta:0. in 
  let s = Mat.of_array [|
    [|0.4 ; 0.  ; 0. |];
    [|0.0 ; 0.4 ; 0. |];
    [|0.0 ; 0.  ; 0.01 |];
  |] in  

  (* Build the measurement from 3 [landarmarks] and a true state
     [x']
   *) 
  let x'= Pos2D.create ~x:1. ~y:1. ~theta:(Angle.pi /. 2.) in 
  let landmarks = [
    Pos2D.create ~x:10. ~y:0.   ~theta:0.; 
    Pos2D.create ~x:10. ~y:10.   ~theta:0.; 
    Pos2D.create ~x:0.  ~y:10. ~theta:0.; 
  ] in 
  let zs = List.map (fun l -> 
    Z.h (l, mat_of_pos x') x'
    (* When invoking [Z.h] it does not matter what the measurement 
       of [z] is. (ie for [z = (l, m)] [m] is ignored in [Z.h].
     *) 
  ) landmarks in 

  (* Perform the correct step repeatedly *)

  let json_list, x,s = Util.fold_n (fun (json_list, x, s) -> 
    let x, s = List.fold_left (fun (x, s) z -> 
      Ekf.correct x s z
    ) (x, s) zs in  
    ((Html.time_i x s)::json_list), x, s
  ) ([], x, s) 100 in 

  (* Printing to file *)

  let json = `List (List.rev json_list) in 
  let out  = open_out "ekf3d_z_only.json" in 
  Yojson.to_channel out json; 
  close_out out; 
  Printf.printf "X: %s\n" @@ Pos2D.to_string x; 
  Mat.print "S:" s;
  () 

let () = 
  (** This example focuses on the [Ekf.predic] methods by repeatedly 
      applying the same command. 

      The display output allows us to see how the uncertainty of the 
      belief (ie covariance [s]) evolves, especially with respect to 
      {ul 
      {- [s]: initial covariance}
      {- [v_noise]: the command noise which gets added to [s] at each step.
      } 
    *)
  let total_time = 10. in 
  let steps      = 100. in 
  
  (* Initial setup of [x] (state) [s] (covariance), [v] command *) 

  let x  = Pos2D.zero in 
  let w  = Angle.pi /. (2. *. total_time)  in 
  let v  = Velocity2D.create ~v:1.  ~w () in 
  let dt = total_time /. steps  in 
  let v_noise = Velocity2D.create_noise 0.1 0.1 0.01 0.01 in 
  let s = Mat.of_array [|
    [|0.1 ; 0.  ; 0. |];
    [|0.0 ; 0.1 ; 0. |];
    [|0.0 ; 0.  ; 0.01 |];
  |] in  

  (* Perform the predict step repeatedly *) 
  
  let json_list, x, s = Util.fold_n (fun (j, x,  s) ->
    let x, s = Ekf.predict x s (v, dt, v_noise) in 
    ((Html.time_i x s) :: j, x, s)
  ) ([], x, s) 100 in 

  let json = `List (List.rev json_list) in 
  let out  = open_out "ekf3d_u_only.json" in 
  Yojson.to_channel out json; 
  close_out out; 
  ()
