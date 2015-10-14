
module L   = Lacaml.D 
module Mat = L.Mat 

module Util = struct 
  let is_square_of_size m n = 
    Mat.dim1 m = n && Mat.dim2 m = n 

  let add_matrix x y = 
    L.gemm ~beta:1. ~c:(L.lacpy y) (Mat.identity (Mat.dim1 x)) x   
    (* very ineficient *)
end 

type dim = int * int 

type t = {
  dim: dim; 
  state_f: Lacaml.D.mat; 
  measurement_f: Lacaml.D.mat; 
  measurement_uncertainty: Lacaml.D.mat; 
}

let create ~dim ~state_f ~measurement_f ~measurement_uncertainty  = 

  let (state_dim, measurement_dim) = dim in 

  begin 
    if not @@ Util.is_square_of_size state_f state_dim 
    then failwith "Invalid state_f dimension"
  end; 
  
  begin 
    if Mat.dim1 measurement_f <> measurement_dim || 
       Mat.dim2 measurement_f <> state_dim
    then failwith "Invalid measurement_f dimension"
  end; 
  
  begin 
    if not @@ Util.is_square_of_size measurement_uncertainty measurement_dim
    then failwith "Invalid measurement_dim dimension"
  end; 
  { dim; state_f; measurement_f; measurement_uncertainty } 


let predict ?u {state_f; dim = (state_d, measurement_d); _ } ~x ~p = 
  begin 
    if not @@ Util.is_square_of_size p state_d
    then failwith "Invalid state uncertainty matrix"
  end; 
  let c = match u with 
    | None   -> Mat.make0 state_d 1
    | Some u -> L.lacpy u 
    (* TODO check size *)
  in   
  let x' = L.gemm ~beta:1. ~c state_f x  in
  let p' = L.gemm (L.gemm state_f p) (Mat.transpose @@ L.lacpy state_f ) in
  (x', p') 

let add_measurement {dim = (state_d, measurement_d); measurement_f; measurement_uncertainty; _ } ~z ~x ~p = 
  begin 
    if Mat.dim1 z <> measurement_d || 
       Mat.dim2 z <> 1 
    then failwith "Invalid measurement matrix" 
  end; 
  let y = L.gemm ~beta:(1.0) ~c:(L.lacpy z) ~alpha:(-. 1.) measurement_f x in 
  let s = L.gemm (L.gemm measurement_f p) (Mat.transpose measurement_f) ~beta:1.0 ~c:(L.lacpy measurement_uncertainty) in 
  L.getri s; 
  let k = L.gemm (L.gemm p (Mat.transpose measurement_f)) s in 
  let x'= Util.add_matrix x (L.gemm k y) in 
  let p'= L.gemm (L.gemm ~beta:1. ~c:(Mat.identity state_d) ~alpha:(-. 1.) k measurement_f) p in 
  (x', p')  
