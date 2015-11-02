type t = {
  x: float; 
  y: float; 
  theta: float; 
}

let create ~x ~y ~theta = {
  x = x; y = y; theta = theta; 
}

let zero = {x = 0.; y = 0.; theta = 0.} 

let average = function 
  | [] -> failwith "Empty list of position for average"
  | ({theta = theta0 ;_}::_) as l  -> 
    let n = float_of_int (List.length l) in 
    let pos = List.fold_left (fun l r -> {
      x = l.x +. r.x;
      y = l.y +. r.y;
      theta = l.theta +.  
        (Angle.normalize @@ r.theta -. theta0 +. Angle.pi) 
        +. theta0 -. Angle.pi
    }) zero l in 
    { 
      x = pos.x /. n ; 
      y = pos.y /. n ; 
      theta = pos.theta /. n; 
    } 

let x {x; _ } = x 

let y {y; _ } = y 

let theta {theta; _ } = theta 

let distance ?from:(from = zero) pos = 
  sqrt @@ ((pos.x -. from.x) ** 2.)  +. ((pos.y -. from.y) ** 2.)

let bearing ?from:({x;y;theta} = zero)  {x=x';y=y'; _ } = 
  Angle.normalize @@ atan2 (y' -. y) (x' -. x) -. theta 

let to_string ?excel {x;y;theta; } = 
  match excel with 
  | None    -> Printf.sprintf 
    "x: %10.3f , y: %10.3f, theta: %7.4f" 
    x y (Angle.degrees_of_radians theta)
  | Some () -> Printf.sprintf 
    "%10.3f, %10.3f, %7.4f" x y theta
