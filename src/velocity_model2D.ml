

module Velocity = struct 
  type t = {
    v: float;
    w: float;
  }

  let create ?v:(v = 0.) ?w:(w = 0.) () = 
    {v; w}

end

module Pos = struct 
  type t = {
    x: float; 
    y: float;
    theta: float;
  }

  let x {x; _ } = x 
  let y {y; _ } = y 
  let theta {theta; _ } = theta 

  let create ?x:(x = 0.) ?y:(y = 0.) ?theta:(theta = 0.) () =
    { x; y ;theta }
end 

let update_pos {Velocity.v; w; } dt {Pos.x; y; theta; } = 
  if Util.within_n_eps ~n:10_000 0. w 
  then {
    Pos.x = x +. v *. dt *. (cos theta) ; 
    Pos.y = y +. v *. dt *. (sin theta) ; 
    Pos.theta; 
  }
  else {
    Pos.x = x -. (v *. (sin theta) /. w) +.  (v *. (sin (theta +. (w *. dt))) /. w); 
    Pos.y = y +. (v *. (cos theta) /. w) -.  (v *. (cos (theta +. (w *. dt))) /. w); 
    Pos.theta = theta +. (w *. dt); 
  }
