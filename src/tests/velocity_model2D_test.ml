
module Vm  = Velocity2D 
module Mat = Matrix_util 

let () = 
  (* Check that a zero velocity has no effect on the position
   *)
  let p = Pos2D.zero in 
  let v = Vm.create () in
  let p = Vm.update_pos v 1.  p in 
  assert(Util.within_n_eps ~n:10 0. (Pos2D.x p)); 
  assert(Util.within_n_eps ~n:10 0. (Pos2D.y p)); 
  assert(Util.within_n_eps ~n:10 0. (Pos2D.theta p)); 
  () 

let () = 
  (* Check that a constant translational motion only, 
   * affects solely the x position. 
   *
   * this tests the special case where the rotational speed is close to 
   * zero.
   *)
  let p = Pos2D.zero in 
  let v = Vm.create ~v:1. () in
  let p = Vm.update_pos v 1.  p in 
  assert(Util.within_n_eps ~n:10 1. (Pos2D.x p)); 
  assert(Util.within_n_eps ~n:10 0. (Pos2D.y p)); 
  assert(Util.within_n_eps ~n:10 0. (Pos2D.theta p)); 
  () 

let () = 
  (* Similar as previous test but this time theta points
   * in the y direction; therefore only y should be affected. 
   *) 
  let p = Pos2D.create ~x:0. ~y:0. ~theta:(Angle.pi /. 2.) in 
  let v = Vm.create ~v:1. () in
  let p = Vm.update_pos v 1.  p in 
  assert(Util.within_n_eps ~n:10 0. (Pos2D.x p)); 
  assert(Util.within_n_eps ~n:10 1. (Pos2D.y p)); 
  assert(Util.within_n_eps ~n:10 (Angle.pi /. 2.)  (Pos2D.theta p)); 
  () 

let () = 
  (* Test that no translational velocity does not affects 
   * the x/y position; only theta is affected. 
   *)
  let p = Pos2D.zero in 
  let v = Vm.create ~w:1. () in
  let p = Vm.update_pos v 1.  p in 
  assert(Util.within_n_eps ~n:10 0. (Pos2D.x p)); 
  assert(Util.within_n_eps ~n:10 0. (Pos2D.y p)); 
  assert(Util.within_n_eps ~n:10 1. (Pos2D.theta p)); 
  () 

let () = 
  (* Test that if the robot travels in a semi circle it's 
   * position is on the y axis is matching the distance it traveled
   * thanks to the equation `distance = pi * radius`
   *)
  let p        = Pos2D.zero in 
  let distance = 1.   in 
  let steps    = 100 in  

  let v = Vm.create ~v:(distance /. float_of_int steps) ~w:(Angle.pi /.  float_of_int steps) () in
  let p = Util.fold_n (fun p -> 
    let p = Vm.update_pos v 1. p in 
    (*
    Printf.printf "%10.5f, %10.5f, %10.5f \n" 
      (Pos2D.x p) 
      (Pos2D.y p) 
      (Pos2D.theta p); 
    *)
    p 
  ) p steps in 
  assert(Util.within_n_eps ~n:100 Angle.pi (Pos2D.theta p)); 
  assert(Util.within_n_eps ~n:100 distance (Angle.pi *. (Pos2D.y p) /. 2.)); 
  () 

let () = 
  (* Debugging test to see the correct 
   * transition from the special handling of the small rotation speed
   * and regular formula.
   *)
  ignore @@ Util.fold_n (fun w -> 
    let w = w +. 100000. *. epsilon_float in 
    let v = Vm.create ~v:1. ~w () in 
    let _ = Vm.update_pos v 1. Pos2D.zero in 
    (*
    Printf.printf "%10.8f, %10.8f, %10.8f \n" 
      (Pos2D.x p) 
      (1_000_000_000. *. Pos2D.y p) 
      (Pos2D.theta p); 
    *)
    w
  ) 0. 10000;
  () 

let () = 
  (* Check that the jacobian for a translation velocity only 
   * is only affecting the x direction. 
   *)
  let p = Pos2D.zero in 
  let v = Vm.create ~v:1. () in 
  Mat.print "G" @@ Vm.pos_jacobian v 1. p;
  ()

