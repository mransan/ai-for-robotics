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

  type z = (float * float * float)
  (** (landmark position , actual measurement, noise) *)
 
  let h (l, m, n) x = 
    (l, l -. x, n)  

  let mat_of_z (_ , m, _ ) = mat_of_float m

  let jacobian _ _ = Mat.of_array [|[| -. 1. |]|]

  let q (_,_, noise)  _ = Mat.of_array [| [| noise |] |]  
end 

module E = Ekf.Make(X)(U)(Z) 

let () = 
  Random.self_init ();

  let total_time = 10. in 
  let steps      = 100. in 

  let x = 0. in 
  let s = Mat.of_array [| [| 0.1 |] |] in

  let dt = total_time /. steps in 

  let u_belief = (1.0, dt, 0.2) in 
  let u_actual = (0.9, dt, 0.0) in 

  let landmark = 11. in 
  let z_noise  = 0.05 in  
  let measurement_every = 4 in 
  
  (* Create a scenario 

     This step creates a scenario which is sequence 
     of motion and measurements. 
     
     
     The scenario motion [u_actual] is different 
     from the [u_belief] by a constant; hence illustrating a 
     constant malfunction of the motor.

     For this scenario each measurement is perfect. However the 
     model for measurement does include noise (ie the [Ekf.correct]
     steps does assume each measurement has a certain noise).
   *)
 
  let events, x = Util.fold_n (fun (events, x) -> 
    let events = (`Motion, x)::events in 
    let x      = U.g x u_actual in 
    if Random.int measurement_every = 0 
    then 
       let m = landmark -. x in 
       (* Sometime we might want to add some random noise 
          to each measurement. In this case we would add the 
          following step:
         
          [let m = m +. Gaussian1D.random z_noise in]
        *)
       ((`Measurement m, x)::events, x)
    else 
       (events, x)
  ) ([], x) 100 in 

  let _, _, log = List.fold_left (fun (x, s, log) (e, xtruth) -> 

    match e with 
    | `Motion -> 
        let log  = (x, xtruth, float_of_mat s ) :: log in 
        let x, s = E.predict x s u_belief  in 
        (x, s, log)
    | `Measurement m -> 
        let log  = (x, xtruth, float_of_mat s) :: log in 
        let x, s = E.correct x s (landmark, m, z_noise) in 
        (x, s, log) 
  ) (0., s, []) (List.rev events) in 

  
  let json = List.rev_map (fun (x, xtruth, s) -> 
    let g = Gaussian1D.create ~mean:x ~variance:s in 
    let precision = 0.3 in 
    let json, _ = Util.fold_n (fun (line_points, x) -> 
      let x = x +. s *. precision in 
      let y = Gaussian1D.x g x in 
      (`List [`Float x ; `Float y])::line_points, x 
    ) ([], (float_of_int (-50)) *. s *. precision  +. x) 101 in  
    `List [`Float xtruth; `Float x; `List (List.rev json)] 
  ) log in 
  let json = `List json in  

  let out = open_out "ekf1d.json" in 
  Yojson.to_channel out json;
  close_out out;
  ()
