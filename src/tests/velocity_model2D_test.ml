
module Vm  = Velocity_model2D
module Mat = Matrix_util 

let () = 
  (* Check that a zero velocity has no effect on the position
   *)
  let p = Vm.Pos.create () in 
  let v = Vm.Velocity.create () in
  let p = Vm.update_pos v 1.  p in 
  assert(Util.within_n_eps ~n:10 0. (Vm.Pos.x p)); 
  assert(Util.within_n_eps ~n:10 0. (Vm.Pos.y p)); 
  assert(Util.within_n_eps ~n:10 0. (Vm.Pos.theta p)); 
  () 

let () = 
  (* Check that a constant translational motion only, 
   * affects solely the x position. 
   *
   * this tests the special case where the rotational speed is close to 
   * zero.
   *)
  let p = Vm.Pos.create () in 
  let v = Vm.Velocity.create ~v:1. () in
  let p = Vm.update_pos v 1.  p in 
  assert(Util.within_n_eps ~n:10 1. (Vm.Pos.x p)); 
  assert(Util.within_n_eps ~n:10 0. (Vm.Pos.y p)); 
  assert(Util.within_n_eps ~n:10 0. (Vm.Pos.theta p)); 
  () 

let () = 
  (* Similar as previous test but this time theta points
   * in the y direction; therefore only y should be affected. 
   *) 
  let p = Vm.Pos.create ~theta:(Angle.pi /. 2.) () in 
  let v = Vm.Velocity.create ~v:1. () in
  let p = Vm.update_pos v 1.  p in 
  assert(Util.within_n_eps ~n:10 0. (Vm.Pos.x p)); 
  assert(Util.within_n_eps ~n:10 1. (Vm.Pos.y p)); 
  assert(Util.within_n_eps ~n:10 (Angle.pi /. 2.)  (Vm.Pos.theta p)); 
  () 

let () = 
  (* Test that no translational velocity does not affects 
   * the x/y position; only theta is affected. 
   *)
  let p = Vm.Pos.create () in 
  let v = Vm.Velocity.create ~w:1. () in
  let p = Vm.update_pos v 1.  p in 
  assert(Util.within_n_eps ~n:10 0. (Vm.Pos.x p)); 
  assert(Util.within_n_eps ~n:10 0. (Vm.Pos.y p)); 
  assert(Util.within_n_eps ~n:10 1. (Vm.Pos.theta p)); 
  () 

let () = 
  (* Test that if the robot travels in a semi circle it's 
   * position is on the y axis is matching the distance it traveled
   * thanks to the equation `distance = pi * radius`
   *)
  let p        = Vm.Pos.create () in 
  let distance = 1.   in 
  let steps    = 100 in  

  let v = Vm.Velocity.create ~v:(distance /. float_of_int steps) ~w:(Angle.pi /.  float_of_int steps) () in
  let p = Util.fold_n (fun p -> 
    let p = Vm.update_pos v 1. p in 
    (*
    Printf.printf "%10.5f, %10.5f, %10.5f \n" 
      (Vm.Pos.x p) 
      (Vm.Pos.y p) 
      (Vm.Pos.theta p); 
    *)
    p 
  ) p steps in 
  assert(Util.within_n_eps ~n:100 Angle.pi (Vm.Pos.theta p)); 
  assert(Util.within_n_eps ~n:100 distance (Angle.pi *. (Vm.Pos.y p) /. 2.)); 
  () 

let () = 
  (* Debugging test to see the correct 
   * transition from the special handling of the small rotation speed
   * and regular formula.
   *)
  ignore @@ Util.fold_n (fun w -> 
    let w = w +. 100000. *. epsilon_float in 
    let v = Vm.Velocity.create ~v:1. ~w () in 
    let _ = Vm.update_pos v 1. (Vm.Pos.create ())  in 
    (*
    Printf.printf "%10.8f, %10.8f, %10.8f \n" 
      (Vm.Pos.x p) 
      (1_000_000_000. *. Vm.Pos.y p) 
      (Vm.Pos.theta p); 
    *)
    w
  ) 0. 10000;
  () 


let print_vec name a =  
    Format.printf "@[<2>%s =@\n@\n@[%a@]@]@\n@\n%!" name Lacaml.Io.pp_fvec a

let () = 
  (* Check that the jacobian for a translation velocity only 
   * is only affecting the x direction. 
   *)
  let p = Vm.Pos.create () in 
  let v = Vm.Velocity.create ~v:1. () in 
  (* Mat.print "G" @@ Vm.pos_jacobian v 1. p;
   *)
  ()

let s0 = Mat.of_array [|
  [| 0.1; 0. ; 0.0  |];
  [| 0. ; 0.1; 0.0  |];
  [| 0. ; 0. ; 0.1  |];
|]

let () = 
  let p = Vm.Pos.create () in 
  let v = Vm.Velocity.create ~v:(Angle.pi *. 2.5 /. 200.) ~w:( Angle.pi/. 200.) () in 
  let v_noise = Vm.Velocity.create_noise 0.2 0.2 0.2 0.2 in 

  let json, _ , _ = Util.fold_n (fun (json, p, s) -> 
    let x = Vm.Pos.x p in 
    let y = Vm.Pos.y p in 
    let (values, vectors) = Mat.eigen_decomposition ~n:2 s in 
    let values = Lacaml_D.Vec.to_array values in 
    let rx = values.(0) in 
    let ry = values.(1) in 
    let vectors = Lacaml_D.Mat.to_array @@ vectors in
    let theta = atan2 vectors.(0).(1) vectors.(0).(0) in 

    let json = 
      (Printf.sprintf "[%10.8f, %10.8f, %10.8f, %10.8f, %10.8f, %10.8f]" 
        x y (Vm.Pos.theta p) (Angle.degrees_of_radians theta  ) rx ry)::json in 

    let p, s = Vm.kf v v_noise 1. p s in 
    (json, p, s) 
  ) ([], p, s0) 1000 in 

  print_endline "[";
  print_endline @@ String.concat ",\n" @@ List.rev json; 
  print_endline "]";
  ()
