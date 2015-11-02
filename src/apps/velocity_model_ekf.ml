
module Vm  = Velocity2D
module Mat = Matrix_util 
module Vec = Vector_util

let s0 = Mat.of_array [|
  [| 0.1; 0. ; 0.0  |];
  [| 0. ; 0.1; 0.0  |];
  [| 0. ; 0. ; 0.1  |];
|]

let () = 
  let out = open_out "velocity_model_ekf.json" in 
  let p = Pos2D.zero in 
  let v = Vm.create ~v:(Angle.pi *. 2.5 /. 200.) ~w:( Angle.pi/. 200.) () in 
  let v_noise = Vm.create_noise 0.2 0.2 0.2 0.2 in 

  let json, _ , _ = Util.fold_n (fun (json, p, s) -> 
    let x = Pos2D.x p in 
    let y = Pos2D.y p in 
    let (values, vectors) = Mat.eigen_decomposition ~n:2 s in 
    let values = Vec.to_array values in 
    let rx = values.(0) in 
    let ry = values.(1) in 
    let vectors = Mat.to_array vectors in
    let theta = atan2 vectors.(0).(1) vectors.(0).(0) in 

    let timei_json = `List [
      `Float x; 
      `Float y;
      `Float (Pos2D.theta p);
      `Float (Angle.degrees_of_radians theta);
      `Float rx;
      `Float ry;
    ] in 
    let json = timei_json::json  in 
    let p, s = Vm.kf v v_noise 1. p s in 
    (json, p, s) 
  ) ([], p, s0) 1000 in 

  let json = `List (List.rev json) in 
  Yojson.to_channel out json; 
  ()
