module Sr = Steering_robot 

let measurement_probability config pos (landmark, measurement) = 
  let predicted = Sr.bearing pos landmark in 
  let error_bearing = abs_float @@ predicted -. measurement in 
  let error_bearing = (Angle.normalize (error_bearing +. Angle.pi)) -. Angle.pi in 
  predicted, error_bearing, (Gaussian1D.x (Sr.Config.bearing_noise config) error_bearing)

let () = 
  Random.self_init ()

let () =
  (** Test the robot motion (ie position update) with no noise *) 

  let config = Sr.Config.create ~length:20. () in 
  let pos   = Pos2D.zero in 
  assert(0. = Pos2D.x pos); 
  assert(0. = Pos2D.y pos); 
  assert(0. = Pos2D.theta pos); 

  let pos = Sr.update_pos config pos (Sr.Motion.create ~steering:0.  ~distance:1.) in  
  
  assert(1. = Pos2D.x pos); 
  assert(0. = Pos2D.y pos); 
  assert(0. = Pos2D.theta pos); 
  
  let pos    = Pos2D.create ~x:0. ~y:0. ~theta:(Angle.pi /. 2.)  in 
  let pos = Sr.update_pos config pos (Sr.Motion.create ~steering:0.  ~distance:1.) in  
  
  assert(Util.within_n_eps 0. (Pos2D.x pos));
  assert(1. = Pos2D.y pos); 
  assert((Angle.pi /. 2.)  = Pos2D.theta pos); 
 
  let motion = Sr.Motion.create ~steering:(Angle.pi /. 4.) ~distance:1. in 

  ignore @@ Util.fold_n (fun pos -> 
    if Pos2D.theta pos > Angle.pi 
    then (assert (Pos2D.x pos < 0.); pos) 
    else Sr.update_pos config pos motion
  ) Pos2D.zero 150

let () = 
  (* Test distance *) 
  let pos = Pos2D.create ~x:1. ~y:0. ~theta:0. in 
  assert(Util.within_n_eps 1. @@ Pos2D.distance pos);
  let pos = Pos2D.create ~x:0. ~y:1. ~theta:0. in 
  assert(Util.within_n_eps 1. @@ Pos2D.distance pos);
  let pos = Pos2D.create ~x:1. ~y:1. ~theta:0. in 
  assert(Util.within_n_eps (sqrt 2.)  @@ Pos2D.distance pos);
  let pos = Pos2D.create ~x:2. ~y:1. ~theta:0. in 
  assert(Util.within_n_eps (sqrt 5.)  @@ Pos2D.distance pos);
  let pos = Pos2D.create ~x:2. ~y:2. ~theta:0. in 
  assert(Util.within_n_eps (sqrt 8.)  @@ Pos2D.distance pos);
  ()


let () = 
  (* Gaussian1D test *) 

  let g = Gaussian1D.create ~mean:10. ~variance:2. in 
  let samples = Util.fold_n (fun l -> 
    (Gaussian1D.random g)::l
  ) [] 10000 in 
  let g' = Gaussian1D.from_samples samples in 
  assert(Gaussian1D.mean g'  < 10.2); 
  assert(Gaussian1D.mean g'  > 9.8); 
  assert(Gaussian1D.variance g'  < 2.1); 
  assert(Gaussian1D.variance g'  > 1.9); 
  ()


let () = 
  (* Make sure that the distance noise is respected *)

  let distance = 10. in  

  let config = Sr.Config.create ~distance_noise:1. ~length:20. ()  in 
  let m      = Sr.Motion.create ~distance  ~steering:0. in 
  let positions = Util.fold_n (fun l -> 
    (Sr.update_pos ~with_noise:() config Pos2D.zero m)::l
  ) [] 10000 in 

  let expected  = Pos2D.create ~x:distance ~y:0. ~theta:0. in 
  let distances = List.map (fun pos -> 
    assert (Util.within_n_eps 0. @@ Pos2D.y pos); 
    assert (Util.within_n_eps 0. @@ Pos2D.theta pos); 
    
    let sign = if Pos2D.x pos > distance then 1. else -. 1. in 
    sign *. Pos2D.distance ~from:expected pos
  ) positions in 

  let g = Gaussian1D.from_samples distances in 
  assert(Util.within ~delta:0.2 0. @@ Gaussian1D.mean g ); 
  assert(Util.within ~delta:0.1 1. @@ Gaussian1D.variance g);
  ()

let () = 
  (* Bearing test *) 

  let landmark = Pos2D.create ~x:1. ~y:1. ~theta:0. in 
  assert(Util.within_n_eps ~n:10 (Angle.pi /. 4.) (Sr.bearing Pos2D.zero landmark)); 
  
  let landmark = Pos2D.create ~x:1. ~y:0. ~theta:0. in 
  assert(Util.within_n_eps ~n:10 (0.) (Sr.bearing Pos2D.zero landmark)); 
  
  let landmark = Pos2D.create ~x:0. ~y:1. ~theta:0. in 
  assert(Util.within_n_eps ~n:10 (Angle.pi /. 2.) (Sr.bearing Pos2D.zero landmark)); 
  
  let landmark = Pos2D.create ~x:(-. 1.)  ~y:0. ~theta:0. in 
  assert(Util.within_n_eps ~n:10 (Angle.pi) (Sr.bearing Pos2D.zero landmark)); 
  
  let landmark = Pos2D.create ~x:0.  ~y:(-. 1.) ~theta:0.  in 
  assert(Util.within_n_eps ~n:10 (1.5 *. Angle.pi) (Sr.bearing Pos2D.zero landmark))

let () = 
  (** This example move the robot once from the zero position and then measures 
      the bearing of various landmark. 

      The above sequence is repeated many time.
   *)

  let out = open_out "measure1.csv" in 

  let config = Sr.Config.create 
    ~steering_noise:0.0 
    ~distance_noise:0.0 
    ~bearing_noise:0.1  ~length:20. () in 

  Util.fold_n(fun () -> 
    let l1  = Pos2D.create ~x:1. ~y:1. ~theta:0. in 
    let l2  = Pos2D.create ~x:0. ~y:1. ~theta:0. in 
    let m   = Sr.Motion.create ~steering:0. ~distance:1. in 
    let pos = Sr.update_pos ~with_noise:() config Pos2D.zero m in 
    let (bearing1, e1, p1) = measurement_probability config pos (l1, (Angle.pi /. 2.)) in 
    let (bearing2, e2, p2) = measurement_probability config pos (l2, (Angle.pi *. 0.75 )) in 

    Printf.fprintf out "%s, %f, %f, %f, %f, %f \n" 
     (Pos2D.to_string pos) 
     (Angle.degrees_of_radians bearing1) p1  
     (Angle.degrees_of_radians bearing2) p2  
     (p1 *. p2)
  ) () 10; 
  close_out out

