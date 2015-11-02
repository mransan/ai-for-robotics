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

type landmark = Pos2D.t 


let update_pos ?with_noise ({Config.length; } as config) p motion = 
  let {Motion.distance; steering} = match with_noise with 
    |  None    -> motion 
    |  Some () -> Motion.add_noise_to_robot_motion config motion 
  in 
  let beta  = distance /. length *. tan (steering) in 
  let theta = Pos2D.theta p in  
  match beta with 
  | beta when beta < 0.001 && beta > (-. 0.001) -> 
    let x = Pos2D.x p  +. distance *. cos theta in 
    let y = Pos2D.y p  +. distance *. sin theta in 
    let theta = Pos2D.theta p in 
    Pos2D.create ~x ~y ~theta 
  | beta -> (
    let r  = distance /. beta in 
    let cx = Pos2D.x p -. (sin theta *. r) in 
    let cy = Pos2D.y p +. (cos theta *. r) in 
    let x = cx +. (sin (theta +. beta)) *. r in
    let y = cy -. (cos (theta +. beta)) *. r in 
    let theta = normalize (theta +. beta) in 
    Pos2D.create ~x ~y ~theta 
  )

type bearing = float 

let add_noise_to_bearing {Config.bearing_noise; _} bearing :float = 
  bearing +. Gaussian1D.random bearing_noise

let bearing ?with_noise p l: bearing = 
  let calculated = Pos2D.bearing ~from:p l in  
  match with_noise with
  | None   -> normalize @@ calculated 
  | Some config -> normalize @@ add_noise_to_bearing config calculated 
