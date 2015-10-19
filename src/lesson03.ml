
let pi = 4. *. atan 1.0 

let normalize x = mod_float (x +. 2. *. pi) (2. *. pi)  

type robot_confguration = {
  length : float; 
  bearing_noise : Gaussian1D.t ; 
}

let robot_confguration ~length ~bearing_noise = {
  length; 
  bearing_noise = Gaussian1D.create ~mean:0. ~variance:(bearing_noise**2.)
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

let string_of_robot_pos {r_x;r_y;r_theta; } = 
  Printf.sprintf "x: %10.3f , y: %10.3f, theta: %7.4f" r_x r_y r_theta

let update_pos {length} {r_x;r_y;r_theta} {steering; distance} = 
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

let bearing {r_x;r_y; r_theta} {l_x; l_y} : bearing = 
  atan2 (l_y -. r_y) (l_x -. r_x) -. r_theta

let measurement_probability {bearing_noise; _ } pos measurements = 
  List.fold_left (fun error (landmark, measurement) -> 
    let predicted = bearing pos landmark in 
    let error_bearing = abs_float @@ predicted -. measurement in 
    let error_bearing = normalize (error_bearing +. pi) -. pi in 
    error *. Gaussian1D.x bearing_noise error_bearing 
  ) 1. measurements
    

(* resampling unit test *) 

let () = 
  Random.self_init (); 
  let gaussian = Gaussian1D.create ~mean:101.5 ~variance:10. in 
  let add_weight = List.map (fun x -> 
    (x, Gaussian1D.x gaussian (float_of_int @@ Char.code x))) 
  in 
  let p = ['a'; 'b'; 'c'; 'd'; 'e'; 'f' ; 'g'; 'h'; 'i'; 'j'; 'k'] in 
  let p = p @ p @ p @ p  in 
  let rec loop (acc: char list) = function 
    | 0 -> acc 
    | n -> (
      print_endline  @@ String.concat "," (List.map Char.escaped acc); 
      loop (Particle_filter.sample (add_weight acc)) (n - 1) 
    )
  in  
  ignore @@ loop p 50



(* ------------------------------------------------- *)


let () = 
  let motions = [
    {steering = 0.        ; distance = 10.}; 
    {steering = pi /. 6.  ; distance = 10.}; 
    {steering = 0.        ; distance = 20.}; 
  ] in 

  let config = robot_confguration ~length:20. ~bearing_noise:0.1 in
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
  () 


