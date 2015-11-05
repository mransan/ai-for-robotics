module V2D  = Velocity2D
module Mat = Matrix_util 
module Vec = Vector_util

let s0 = Mat.of_array [|
  [| 0.1; 0. ; 0.0   |];
  [| 0. ; 0.1; 0.0   |];
  [| 0. ; 0. ; 0.005 |];
|]

let landmarks = [
  Pos2D.create ~x:10.      ~y:10. ~theta:0.; 
  Pos2D.create ~x:0.       ~y:10. ~theta:0.; 
  Pos2D.create ~x:(-. 10.) ~y:5.  ~theta:0.; 
]

let zp p l = Mat.of_array [|
  [| Pos2D.distance ~from:p l |]; 
  [| Pos2D.bearing  ~from:p l |]; 
  [| 1.                       |];
|]

let h {Pos2D.x; y; _} {Pos2D.x = l_x; y = l_y; _ } = 
  let q = (l_x -. x)**2. +. (l_y -. y)**2. in 
  Mat.of_array [|
    [|-. (l_x -. x) /. sqrt q ; -. (l_y -. y)/.sqrt q;  0.  |];
    [|   (l_y -. y) /.      q ; -. (l_x -. x)/.     q; -1.  |];
    [|        0.              ;         0.           ;  0.  |];
  |] 

let q = Mat.of_array [|
  [| 0.003  ; 0.     ; 0.  |]; 
  [| 0.     ; 0.0001 ; 0.  |]; 
  [| 0.     ; 0.     ; 0.00001  |]; 
|]

let mat_of_pos {Pos2D.x;y;theta} = Mat.of_array [| 
  [| x |];
  [| y |];
  [| theta |];
|]

let pos_of_mat m = 
  let a = Mat.to_array m in 
  {Pos2D.x = a.(0).(0); y = a.(1).(0); theta = a.(2).(0); } 

let kf_update p s l z q = 
  let zp = zp p l in 
  let h  = h  p l in 

  let open Mat.Ops in 
  let si = h *~ s *~ (Mat.transpose h) +~ q in 
  Lacaml_D.getri si;
  let ki = s *~ (Mat.transpose h) *~ si in 
  let p  = mat_of_pos p  +~ ki *~ (z -~ zp) in  
  let s  = (Mat.identity 3 -~ ki *~h) *~ s in 
  (pos_of_mat p, s)

let () = 
  
  let p  = Pos2D.zero in 
  
  let ap = Pos2D.create ~x:5. ~y:0. ~theta:0.1 in 
  let s  = 
    let s = float_of_string Sys.argv.(1) in 
    Mat.of_array [|
      [|s   ; 0.  ; 0.  ;|];
      [|0.  ; s   ; 0.  ;|];
      [|0.  ; 0.  ; s   ;|];
    |] in 

  let l1 = Pos2D.create ~x:1.  ~y:1. ~theta:0. in
  let z1 = zp ap l1 in 
  let l2 = Pos2D.create ~x:(-. 1.)  ~y:1. ~theta:0. in
  let z2 = zp ap l2 in 
  let q  = 
    let q = float_of_string Sys.argv.(2) in 
    Mat.of_array [|
      [| q  ; 0.  ; 0.  |]; 
      [| 0. ; q   ; 0.  |]; 
      [| 0. ; 0.  ; q /. 2.   |]; 
    |]
  in 

  print_endline "Before Kalman Update";
  Mat.print "Belief pos" (mat_of_pos p); 
  Mat.print "Actual pos" (mat_of_pos ap); 
  Mat.print "Covariance pos" s; 
  
  print_endline "Measurement:";
  Mat.print "From actual (no noise)" z1;
  Mat.print "Estimated (from belief)" (zp p l1);

  let (p, s) = kf_update p s l1 z1 q in 

  print_endline "After Kalman Update";
  Mat.print "Belief pos" (mat_of_pos p); 
  Mat.print "Covariance pos" s; 
  
  print_endline "Measurement:";
  Mat.print "From actual (no noise)" z2;
  Mat.print "Estimated (from belief)" (zp p l2);

  let (p, s) = kf_update p s l2 z2 q in 
  
  print_endline "After Kalman Update";
  Mat.print "Belief pos" (mat_of_pos p); 
  Mat.print "Covariance pos" s; 
  let (values, vectors) = Mat.eigen_decomposition ~n:3 s in 
  Vec.print "Eigen values" values;
  Mat.print "Eigen vectors" vectors;
  ()




(* 

let () = 
  let out = open_out "velocity_model_ekf.json" in 
  let p = Pos2D.zero in 
  let v = V2D.create ~v:(Angle.pi *. 2.5 /. 200.) ~w:( 0. *. Angle.pi/. 200.) () in 
  let v = V2D.create ~v:(5. /. 100.) ~w:0. () in 
  let v_noise = V2D.create_noise 0.4 0.4 0.2 0.2 in 

  let n  = 100 in 
  let dt = 1. in 
  let vn = {
    V2D.v = 5. /. 100.; 
    V2D.w = 0.01;
  } in 
 
  let events, _, pn, _  = Util.fold_n (fun (events, p, pn, i) -> 
    let {V2D.v = v'; w = w'} = v in 
    let p  = V2D.update_pos v  dt p  in 
    let pn = V2D.update_pos vn dt pn in 
    let events = (`Motion (v, vn, p, pn))::events in 
    let events = List.fold_left (fun events l -> 
      (`Measurement (l, zp pn l)) :: events
    ) events landmarks in  
    (events, p, pn, i+1)
  ) ([], p, p, 0) n in 


  let json, _ , _ = List.fold_left (fun (json, p, s) event -> 
    let x = Pos2D.x p in 
    let y = Pos2D.y p in 
    let (values, vectors) = Mat.eigen_decomposition ~n:2 s in 
    let values = Vec.to_array values in 
    let rx = values.(0) in 
    let ry = values.(1) in 
    let vectors = Mat.to_array vectors in
    let theta = atan2 vectors.(0).(1) vectors.(0).(0) in 

    match event with 
    |`Motion (v, vn, p, pn) -> 
      let timei_json = `List [
        `Float x; 
        `Float y;
        `Float (Pos2D.theta p);
        `Float (Pos2D.x pn); 
        `Float (Pos2D.y pn);
        `Float (Pos2D.theta pn);
        `Float (Angle.degrees_of_radians theta);
        `Float rx;
        `Float ry;
      ] in 
      let json = timei_json::json  in 
      let p, s = V2D.kf v v_noise 1. p s in 
      (json, p, s) 
    | `Measurement (l, z) -> 
      let p, s = kf_update p s l z in 
      (json, p, s)  
    | _ -> failwith "Error" 
  ) ([], p, s0) (List.rev events) in 

  let json = `List (List.rev json) in 
  Yojson.to_channel out json; 
  ()
*)
