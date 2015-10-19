
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
  x_f: Lacaml.D.mat; 
  z_f: Lacaml.D.mat; 
  z_cov: Lacaml.D.mat; 
}

let create ~dim ~x_f ~z_f ~z_cov= 

  let (x_d, z_d) = dim in 

  begin 
    if not @@ Util.is_square_of_size x_f x_d 
    then failwith "Invalid x_f dimension"
  end; 
  
  begin 
    if Mat.dim1 z_f <> z_d || 
       Mat.dim2 z_f <> x_d
    then failwith "Invalid z_f dimension"
  end; 
  
  begin 
    if not @@ Util.is_square_of_size z_cov z_d
    then failwith "Invalid z_d dimension"
  end; 
  { dim; x_f; z_f; z_cov } 


let update ?x_f ?z_f ?z_cov ({dim = (x_d, z_d); _ } as kf) =
  let kf = match x_f with 
    | Some x_f -> 
      if not @@ Util.is_square_of_size x_f x_d
      then failwith "Invalid x_f size"
      else {kf with x_f}
    | None -> kf in 
  
  let kf = match z_f with 
    | Some z_f -> 
      if Mat.dim1 z_f <> z_d || 
         Mat.dim2 z_f <> x_d
      then failwith "Invalid z_f dimension"
      else {kf with z_f}
    | None -> kf in 
  
  let kf = match z_cov with 
    | Some z_cov -> 
      if not @@ Util.is_square_of_size z_cov z_d 
      then failwith "Invalid z_cov dimension"
      else {kf with z_cov}
    | None -> kf in 
  kf 

let predict {x_f; dim = (state_d, measurement_d); _ } ~x ~x_cov = 
  (*
  begin 
    if not @@ Util.is_square_of_size p state_d
    then failwith "Invalid state uncertainty matrix"
  end; 
  let c = match u with 
    | None   -> Mat.make0 state_d 1
    | Some u -> L.lacpy u 
    (* TODO check size *)
  in   
  *)
  let c  = Mat.make0 state_d 1 in 
  let x' = L.gemm ~beta:1. ~c x_f x  in
  let x_cov' = L.gemm (L.gemm x_f x_cov) (Mat.transpose @@ L.lacpy x_f ) in
  (x', x_cov') 

let add_measurement {dim = (state_d, measurement_d); z_f; z_cov; _ } ~z ~x ~x_cov = 
  begin 
    if Mat.dim1 z <> measurement_d || 
       Mat.dim2 z <> 1 
    then failwith "Invalid measurement matrix" 
  end; 
  let y = L.gemm ~beta:(1.0) ~c:(L.lacpy z) ~alpha:(-. 1.) z_f x in 
  let s = L.gemm (L.gemm z_f x_cov) (Mat.transpose z_f) ~beta:1.0 ~c:(L.lacpy z_cov) in 
  L.getri s; 
  let k = L.gemm (L.gemm x_cov (Mat.transpose z_f)) s in 
  let x'= Util.add_matrix x (L.gemm k y) in 
  let x_cov'= L.gemm (L.gemm ~beta:1. ~c:(Mat.identity state_d) ~alpha:(-. 1.) k z_f) x_cov in 
  (x', x_cov')  
