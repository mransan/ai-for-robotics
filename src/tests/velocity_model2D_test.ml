
module Vm = Velocity_model2D

let () = 
  let p = Vm.Pos.create () in 
  let v = Vm.Velocity.create () in
  let p = Vm.update_pos v 1.  p in 
  assert(Util.within_n_eps ~n:10 0. (Vm.Pos.x p)); 
  assert(Util.within_n_eps ~n:10 0. (Vm.Pos.y p)); 
  assert(Util.within_n_eps ~n:10 0. (Vm.Pos.theta p)); 
  () 

let () = 
  let p = Vm.Pos.create () in 
  let v = Vm.Velocity.create ~v:1. () in
  let p = Vm.update_pos v 1.  p in 
  assert(Util.within_n_eps ~n:10 1. (Vm.Pos.x p)); 
  assert(Util.within_n_eps ~n:10 0. (Vm.Pos.y p)); 
  assert(Util.within_n_eps ~n:10 0. (Vm.Pos.theta p)); 
  () 

let () = 
  let p = Vm.Pos.create ~theta:(Angle.pi /. 2.) () in 
  let v = Vm.Velocity.create ~v:1. () in
  let p = Vm.update_pos v 1.  p in 
  assert(Util.within_n_eps ~n:10 0. (Vm.Pos.x p)); 
  assert(Util.within_n_eps ~n:10 1. (Vm.Pos.y p)); 
  assert(Util.within_n_eps ~n:10 (Angle.pi /. 2.)  (Vm.Pos.theta p)); 
  () 

let () = 
  let p = Vm.Pos.create () in 
  let v = Vm.Velocity.create ~w:1. () in
  let p = Vm.update_pos v 1.  p in 
  assert(Util.within_n_eps ~n:10 0. (Vm.Pos.x p)); 
  assert(Util.within_n_eps ~n:10 0. (Vm.Pos.y p)); 
  assert(Util.within_n_eps ~n:10 1. (Vm.Pos.theta p)); 
  () 

let () = 
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

