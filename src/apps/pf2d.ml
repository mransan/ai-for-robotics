
module Sr = Steering_robot 

let json_of_pos ?truth pos = 
  let truth = match truth with
    | None -> false 
    | Some () -> true 
  in 
  `List [
    `Float (Pos2D.x pos); 
    `Float (Pos2D.y pos); 
    `Float (Pos2D.theta pos); 
    `Bool  truth; 
  ]

let measurement_probability config pos (landmark, measurement) = 
  let predicted = Sr.bearing pos landmark in 
  let error_bearing = abs_float @@ predicted -. measurement in 
  let error_bearing = (Angle.normalize (error_bearing +. Angle.pi)) -. Angle.pi in 
  predicted, error_bearing, (Gaussian1D.x (Sr.Config.bearing_noise config) error_bearing)

let () = 
  let json_out = open_out "pf2d.json" in  

  let m = Sr.Motion.create ~distance:0.5 ~steering:(Angle.pi *. 0.3 ) in 
  let n = 200 in  
  let l1 = Pos2D.create ~x:3. ~y:3. ~theta:0. in 
  let l2 = Pos2D.create ~x:3. ~y:(- 3.) ~theta:0. in 
  let l3 = Pos2D.create ~x:(-. 6.) ~y:(6.) ~theta:0. in 
  let n_particles = 1000 in 
  let initial_pos_max_d = 4. in 
  let initial_pos_max_t = Angle.pi  in 

  let config = Sr.Config.create 
    ~steering_noise:0.05 
    ~distance_noise:0.1
    ~bearing_noise:0.1  
    ~length:20. () in 
  
  (* Calculate measurements and final pos*)
  
  let pos, measurements  = Util.fold_n (fun (pos, measurements) -> 
    let pos' = Sr.update_pos config pos m in 
    (pos', (pos, (Sr.bearing pos l1, 
                  Sr.bearing pos l2, 
                  Sr.bearing pos l3))::measurements)
  ) (Pos2D.zero, []) n in  

  (* Randomly initialize the particles *)
  
  let particles = 
    let v () = (Random.float initial_pos_max_d  -. (initial_pos_max_d /. 2.) ) in 
    Util.fold_n (fun l -> 
      let pos = Pos2D.create ~x:(v()) ~y:(v()) ~theta:(Random.float initial_pos_max_t)  in 
      pos::l
    ) [] n_particles 
  in 
  
  (** Run the filtering step *)

  let particles, json = List.fold_left  (fun (particles, json) (thruth_pos, (m1, m2, m3)) -> 
    
    let particles_json = List.fold_left (fun particles_json pos -> 
      (json_of_pos pos)::particles_json 
    ) [] particles in 
    
    let particles_json = 
      (json_of_pos ~truth:() thruth_pos)::particles_json in 

    let x_histogram = Gaussian1D.histogram_from_samples 50 (List.map Pos2D.x particles) in 
    let x_histogram_json = List.map (fun (x, y) -> 
      `List [`Float x; `Float y]
    ) x_histogram in  
    
    let y_histogram = Gaussian1D.histogram_from_samples 50 (List.map Pos2D.y particles) in 
    let y_histogram_json = List.map (fun (x, y) -> 
      `List [`Float x; `Float y]
    ) y_histogram in  

    let time_i_json = `Assoc [
      ("particles"     , `List (List.rev particles_json)); 
      ("x_distribution", `List x_histogram_json); 
      ("y_distribution", `List y_histogram_json); 
    ] in 

    let w_particles = List.fold_right (fun pos acc -> 
      let b1, e1, p1 = measurement_probability config pos (l1, m1) in 
      let b2, e2, p2 = measurement_probability config pos (l2, m2) in 
      let b3, e3, p3 = measurement_probability config pos (l3, m3) in 
      (pos, p1 *. p2 *. p3)::acc 
    ) particles [] in 

    let particles = Particle_filter.sample w_particles in 
    let particles = List.map (fun pos -> Sr.update_pos ~with_noise:() config pos m) particles in 
    
    (particles, time_i_json::json)  

  ) (particles,[]) (List.rev measurements) in 

  let json = `List (List.rev json) in  
  Yojson.to_channel json_out json;  
  close_out json_out; 
  () 
