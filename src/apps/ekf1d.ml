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

  let x = 0. in 
  let s = Mat.of_array [| [| 0.1 |] |] in
  let u_gaussian = Gaussian1D.create ~mean:0. ~variance:0.2 in 
  let dt = 1. in 
  let u = (1., dt, (Gaussian1D.variance u_gaussian)) in 
  let landmark = 5. in 
  let z_noise = Gaussian1D.create ~mean:0. ~variance:0.2 in 
  
  (* Create a scenario 

     This step creates a scenario which is sequence 
     of motion and measurements. 
     
     Both motion and measurements are done with noise to simulate
     a realistic scenario.
   *)
 
  let events, x = Util.fold_n (fun (events, x) -> 
    let events = (`Motion, x)::events in 
    let x      = U.g x u in 
    let x      = x +. (Gaussian1D.random u_gaussian) in  
    if Random.int 5 = 0 
    then 
       let m = landmark -. x in 
       let m = m +. Gaussian1D.random z_noise in 
       ((`Measurement m, x)::events, x)
    else 
       (events, x)
  ) ([], x) 100 in 

  let _, _, log = List.fold_left (fun (x, s, log) (e, xtruth) -> 

    match e with 
    | `Motion -> 
        let log  = (x, xtruth, float_of_mat s ) :: log in 
        let x, s = E.predict x s u  in 
        (x, s, log)
    | `Measurement m -> 
        let log  = (x, xtruth, float_of_mat s) :: log in 
        let x, s = E.correct x s (landmark, m, Gaussian1D.variance z_noise) in 
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
