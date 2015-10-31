let pi = 4. *. atan 1.0 

let normalize x = mod_float (x +. 2. *. pi) (2. *. pi)  

let degrees_of_radians x = x *. 180. /. pi

module Config = struct 

  type t = {
    length : float; 
    bearing_noise : Gaussian1D.t ; 
    steering_noise: Gaussian1D.t ; 
    distance_noise: Gaussian1D.t ; 
  }
  
  let create 
    ?distance_noise:(distance_noise = 0.)
    ?steering_noise:(steering_noise = 0.) 
    ?bearing_noise:(bearing_noise = 0.) ~length () = {
    length; 
    bearing_noise = Gaussian1D.create ~mean:0. ~variance:(bearing_noise);
    steering_noise = Gaussian1D.create ~mean:0. ~variance:(steering_noise);
    distance_noise = Gaussian1D.create ~mean:0. ~variance:(distance_noise);
  }

  let bearing_noise {bearing_noise; _ } = bearing_noise

end 

module Motion = struct 

  type t = {
    steering : float; 
    distance : float; 
  }
  
  let add_noise_to_robot_motion {Config.steering_noise; distance_noise} {steering; distance} = {
      steering = steering +. Gaussian1D.random steering_noise; 
      distance = distance +. Gaussian1D.random distance_noise;
    }
  
  let create ~steering ~distance = {steering; distance} 

end 

module Landmark = struct 

  type t = {
    l_x: float; 
    l_y: float; 
  }

  let create ~x ~y = {
    l_x = x; l_y = y; 
  }

end 


module Pos = struct 

  type t = {
    r_x: float; 
    r_y: float; 
    r_theta: float; 
  }
  
  let create ~x ~y ~theta = {
    r_x = x; r_y = y; r_theta = theta; 
  }

  let zero = {r_x = 0.; r_y = 0.; r_theta = 0.} 

  let average = function 
    | [] -> failwith "Empty list of position for average"
    | ({r_theta = r_theta0 ;_}::_) as l  -> 
      let n = float_of_int (List.length l) in 
      let pos = List.fold_left (fun l r -> {
        r_x = l.r_x +. r.r_x;
        r_y = l.r_y +. r.r_y;
        r_theta = l.r_theta +.  
          (Angle.normalize @@ r.r_theta -. r_theta0 +. Angle.pi) 
          +. r_theta0 -. Angle.pi
      }) zero l in 
      { 
        r_x = pos.r_x /. n ; 
        r_y = pos.r_y /. n ; 
        r_theta = pos.r_theta /. n; 
      } 

  let x {r_x; _ } = r_x 

  let y {r_y; _ } = r_y 

  let theta {r_theta; _ } = r_theta 

  let abs_distance ?from:(from = zero) pos = 
    sqrt @@ ((pos.r_x -. from.r_x) ** 2.)  +. ((pos.r_y -. from.r_y) ** 2.)
  
  let to_string ?excel {r_x;r_y;r_theta; } = 
    match excel with 
    | None    -> Printf.sprintf "x: %10.3f , y: %10.3f, theta: %7.4f" r_x r_y (degrees_of_radians r_theta)
    | Some () -> Printf.sprintf "%10.3f, %10.3f, %7.4f" r_x r_y r_theta
end 

let update_pos ?with_noise ({Config.length; } as config) {Pos.r_x;r_y;r_theta} motion = 
  let {Motion.distance; steering} = match with_noise with 
    |  None    -> motion 
    |  Some () -> Motion.add_noise_to_robot_motion config motion 
  in 
  let beta = distance /. length *. tan (steering) in 
  match beta with 
  | beta when beta < 0.001 && beta > (-. 0.001) -> { 
    Pos.r_x = r_x +. distance *. cos r_theta; 
    Pos.r_y = r_y +. distance *. sin r_theta; 
    Pos.r_theta; 
  }
  | beta -> (
    let r  = distance /. beta in 
    let cx = r_x -. (sin r_theta *. r) in 
    let cy = r_y +. (cos r_theta *. r) in {
      Pos.r_x = cx +. (sin (r_theta +. beta)) *. r ;
      Pos.r_y = cy -. (cos (r_theta +. beta)) *. r;
      Pos.r_theta = normalize (r_theta +. beta);
    } 
  )

type bearing = float 

let add_noise_to_bearing {Config.bearing_noise; _} bearing :float = 
  bearing +. Gaussian1D.random bearing_noise

let bearing ?with_noise {Pos.r_x;r_y; r_theta} {Landmark.l_x; l_y} : bearing = 
  let calculated = atan2 (l_y -. r_y) (l_x -. r_x) -. r_theta in 
  match with_noise with
  | None   -> normalize @@ calculated 
  | Some config -> normalize @@ add_noise_to_bearing config calculated 
