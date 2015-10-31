
let pi = 4. *. atan 1.0 

let normalize x = mod_float (x +. 2. *. pi) (2. *. pi)  

let degrees_of_radians x = x *. 180. /. pi

type robot_confguration = {
  length : float; 
  bearing_noise : Gaussian1D.t ; 
  steering_noise: Gaussian1D.t ; 
  distance_noise: Gaussian1D.t ; 
}

let robot_confguration ~length ~bearing_noise = {
  length; 
  bearing_noise = Gaussian1D.create ~mean:0. ~variance:(bearing_noise);
  steering_noise = Gaussian1D.create ~mean:0. ~variance:(0.1 ** 2.); 
  distance_noise = Gaussian1D.create ~mean:0. ~variance:(5.0 ** 2.); 
}

type robot_pos = {
  r_x: float; 
  r_y: float; 
  r_theta: float; 
}

type robot_motion = {
  steering : float; 
  distance : float; 
}

let add_noise_to_robot_motion {steering_noise; distance_noise} {steering; distance} = {
    steering = steering +. Gaussian1D.random steering_noise; 
    distance = distance +. Gaussian1D.random distance_noise;
  }

let string_of_robot_pos ?excel {r_x;r_y;r_theta; } = 
  match excel with 
  | None    -> Printf.sprintf "x: %10.3f , y: %10.3f, theta: %7.4f" r_x r_y r_theta
  | Some () -> Printf.sprintf "%10.3f, %10.3f, %7.4f" r_x r_y r_theta

let update_pos ?with_noise ({length; } as config) {r_x;r_y;r_theta} motion = 
  let {distance; steering} = match with_noise with 
    |  None    -> motion 
    |  Some () -> add_noise_to_robot_motion config motion 
  in 
  let beta = distance /. length *. tan (steering) in 
  match beta with 
  | beta when beta < 0.001 && beta > (-. 0.001) -> { 
    r_x = r_x +. distance *. cos r_theta; 
    r_y = r_y +. distance *. sin r_theta; 
    r_theta; 
  }
  | beta -> (
    let r  = distance /. beta in 
    let cx = r_x -. (sin r_theta *. r) in 
    let cy = r_y +. (cos r_theta *. r) in {
      r_x = cx +. (sin (r_theta +. beta)) *. r ;
      r_y = cy -. (cos (r_theta +. beta)) *. r;
      r_theta = normalize (r_theta +. beta);
    } 
  )

type landmark = {
  l_x: float; 
  l_y: float; 
}

type bearing = float 

let add_noise_to_bearing {bearing_noise; _} bearing :float = 
  bearing +. Gaussian1D.random bearing_noise

let bearing {r_x;r_y; r_theta} {l_x; l_y} : bearing = 
  normalize @@ atan2 (l_y -. r_y) (l_x -. r_x) -. r_theta

let measurement_probability {bearing_noise; _ } pos measurements = 
  List.fold_left (fun (error, s) (landmark, measurement) -> 
    let predicted = bearing pos landmark in 
    let error_bearing = abs_float @@ predicted -. measurement in 
    let error_bearing = (normalize (error_bearing +. pi)) -. pi in 
    let s = s ^ (string_of_float error_bearing) ^ ", " ^ (string_of_float error) ^ ", " in 
    (error *. Gaussian1D.x bearing_noise error_bearing, s) 
  ) (1., "") measurements
    

(* ------------------------------------------------- *)


let () = 
  let motions = [
    {steering = 0.        ; distance = 10.}; 
    {steering = pi /. 6.  ; distance = 10.}; 
    {steering = 0.        ; distance = 20.}; 
  ] in 

  let config = robot_confguration ~length:20. ~bearing_noise:0.5 in
  let pos = { r_x =0.; r_y =0. ; r_theta = 0. } in

  ignore @@ List.fold_left (fun pos motion  -> 
    let pos = update_pos config pos motion in 
    print_endline @@ string_of_robot_pos pos; 
    pos 
  ) pos motions ;
  
  print_endline "------";

  let rec loop pos = function
    | 0 -> ()
    | i -> (
      let pos = update_pos config pos {steering=(-. 0.2); distance = 10. } in
      print_endline @@ string_of_robot_pos pos; 
      loop pos (i - 1)  
    )
  in 
  loop pos 10; 
 
  print_endline "------";

  let landmarks = [ 
    {l_y = 0.    ; l_x = 100. } ; 
    {l_y = 0.    ; l_x = 0. } ; 
    {l_y = 100.  ; l_x = 0. } ; 
    {l_y = 100.  ; l_x = 100. } ; 
  ] in 

  let pos = {r_x = 30. ; r_y = 20.; r_theta =0. } in 
  List.iter (fun landmark -> 
    Printf.printf "%f \n" @@ normalize @@ bearing pos landmark 
  ) landmarks; 
  
  let motion = {
    steering = 2. *. pi /. 10. ; 
    distance = 20.;
  } in
  
  let measurements = [
    [4.746936; 3.859782; 3.045217; 2.045506];
    [3.510067; 2.916300; 2.146394; 1.598332];
    [2.972469; 2.407489; 1.588474; 1.611094];
    [1.906178; 1.193329; 0.619356; 0.807930];
    [1.352825; 0.662233; 0.144927; 0.799090];
    [0.856150; 0.214590; 5.651497; 1.062401];
    [0.194460; 5.660382; 4.761072; 2.471682];
    [5.717342; 4.736780; 3.909599; 2.342536]
  ] in

  let pos = {r_x = 0. ; r_y = 0.; r_theta = 0. } in 
  let nb_particles = 1000 in 
  let particles = 
    let rec loop acc = function 
      | 0 -> acc 
      | i -> 
        let pos = {
          r_x = Random.float 100.; 
          r_y = Random.float 100.; 
          r_theta = Random.float @@ 2. *. pi; 
        } in 
        loop (pos :: acc) (i - 1) 
    in loop [] nb_particles
  in 

  assert (nb_particles = List.length particles); 


  let out = open_out "lesson03.csv" in 
  

  let particles = List.fold_left (fun (particles, i) measurement -> 

    
    let particles = List.map (fun pos -> 
      update_pos config pos @@ add_noise_to_robot_motion config motion
    ) particles in

    let measurement = List.map (add_noise_to_bearing config) measurement in 

    let measurement = List.combine landmarks measurement in 
    
    let weighted_particles = List.map (fun pos ->
      (pos, measurement_probability config pos measurement)  
    ) particles in 
    
    let s = String.concat "\n" (List.map (fun ({r_x; r_y; }, (w, s)) -> 
      Printf.sprintf "%f, %f, %f, %s" r_x r_y w s
    ) weighted_particles) in 
    
    let weighted_particles = List.map (fun (pos, (w, s)) -> (pos, w)) weighted_particles in 
    begin 
      if i = 1
      then Printf.fprintf out "%s" s; 
    end;

    (Particle_filter.sample weighted_particles, i + 1) 
  ) (particles, 0) measurements in 

  let pos = List.fold_left (fun pos particle -> {
    r_x = pos.r_x +. particle.r_x; 
    r_y = pos.r_y +. particle.r_y; 
    r_theta = pos.r_theta +. particle.r_theta; 
  }) pos (fst particles) in 
  
  let pos = {
    r_x = pos.r_x /. (float_of_int nb_particles); 
    r_y = pos.r_y /. (float_of_int nb_particles);  
    r_theta = pos.r_theta /. (float_of_int nb_particles);  
  } in
  Printf.printf "x: %f, y: %f, theta: %f\n" pos.r_x pos.r_y pos.r_theta;  









  () 


