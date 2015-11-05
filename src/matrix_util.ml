module L   = Lacaml.D 
module Mat = Lacaml.D.Mat  


type mat = L.mat 

let cpy  x = 
  L.lacpy x 

let identity = Mat.identity 

let of_array = Mat.of_array 

let to_array = Mat.to_array

let sub_matrix ~n ~m a = 
  L.lacpy ~n ~m a

let add x y = 
  L.gemm ~beta:1. ~c:(L.lacpy y) (Mat.identity (Mat.dim1 x)) x   
  (* very ineficient *)

let sub x y = 
  L.gemm ~beta:(-. 1.)  ~c:(L.lacpy y) (Mat.identity (Mat.dim1 x)) x   

let mul x y = 
  L.gemm x y 

let transpose x = 
  Mat.transpose x 

let eigen_decomposition ?n m = 
  let m' = cpy m in 
  let e  = L.syev ?n ~vectors:true m' in 
  (e, m')

let square_dim m = 
  let r = Mat.dim1 m in 
  let c = Mat.dim2 m in 
  if r <> c 
  then failwith "Non square matrix in square_dim"
  else r

let print name a =  
    Format.printf "@[<2>%s =@\n@\n@[%a@]@]@\n@\n%!" name Lacaml.Io.pp_fmat a

module Ops = struct
  let ( +~ )  = add 
  let ( -~ )  = sub
  let ( *~ )  = mul 
end 
