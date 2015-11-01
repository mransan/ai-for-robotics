module Mat = Matrix_util

module Velocity = struct 
  type t = {
    v: float;
    w: float;
  }

  let create ?v:(v = 0.) ?w:(w = 0.) () = 
    {v; w}
  
  type noise = {
    a1: float;
    a2: float;
    a3: float;
    a4: float;
  }

  let create_noise a1 a2 a3 a4 = {a1;a2;a3;a4}

  let noise_matrix {v;w;} {a1;a2;a3;a4} = Mat.of_array [|
    [|a1 *. (v**2.) +. a2 *. (w ** 2.); 0.                                 |];
    [|0.                              ; a3 *. (v**2.) +. a4 *. (w ** 2.);  |];
  |]

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
    Pos.theta = theta +. (w *. dt); 
  }
  else {
    Pos.x = x -. (v *. (sin theta) /. w) +.  (v *. (sin (theta +. (w *. dt))) /. w); 
    Pos.y = y +. (v *. (cos theta) /. w) -.  (v *. (cos (theta +. (w *. dt))) /. w); 
    Pos.theta = theta +. (w *. dt); 
  } (* TODO factorize like in the jacobian *)


let pos_jacobian {Velocity.v; w; } dt {Pos.x; y; theta; } = 
  if Util.within_n_eps ~n:10_000 0. w 
  then Mat.of_array  [| 
    [|1. ; 0. ; -. v *. dt *. (sin theta)  |] ; 
    [|0. ; 1. ;    v *. dt *. (cos theta)  |] ; 
    [|0. ; 0. ;    1.                      |] ; 
  |]
  else Mat.of_array  [| 
    [|1. ; 0. ; (v /. w) *. (-. (cos theta) +. (cos (theta +. (w *. dt)))); |]; 
    [|0. ; 1. ; (v /. w) *. (-. (sin theta) +. (sin (theta +. (w *. dt)))); |]; 
    [|0. ; 0. ;    1.                                                       |]; 
  |]

let motion_jacobian {Velocity.v; w; } dt {Pos.x; y; theta; } = 
  if Util.within_n_eps ~n:10_000 0. w 
  then Mat.of_array  [| 
    [|  dt *. (cos theta) ; 0. |] ; 
    [|  dt *. (sin theta) ; 0. |] ; 
    [|  0.                ; dt |] ; 
  |]
  else 
    let st  = sin theta in 
    let stt = sin (theta +. (w *. dt)) in 
    let ct  = cos theta in 
    let ctt = cos (theta +. (w *. dt)) in 
    Mat.of_array  [| 
      [| (-. st +. stt) /. w; (  v *. (st -. stt) /. (w ** 2.)) +. (v *. ctt *. dt /. w) |]; 
      [| (   ct -. ctt) /. w; (-.v *. (ct -. ctt) /. (w ** 2.)) +. (v *. stt *. dt /. w) |]; 
      [|0.                  ; dt                                                        |]
    |]

let print_mat name a =  
    Format.printf "@[<2>%s =@\n@\n@[%a@]@]@\n@\n%!" name Lacaml.Io.pp_fmat a

let kf vel vel_noise dt pos pos_noise = 
  let pos' = update_pos vel dt pos in 
  let g    = pos_jacobian vel dt pos in 
  let m    = Velocity.noise_matrix vel vel_noise in 
  let v    = motion_jacobian vel dt pos in  

  let s1 = Mat.mul (Mat.mul g pos_noise) (Mat.transpose g) in 
  let s2 = Mat.mul (Mat.mul v m ) (Mat.transpose v) in 
  pos', Mat.add s1 s2 
