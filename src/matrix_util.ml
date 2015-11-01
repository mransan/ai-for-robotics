

module L   = Lacaml.D 
module Mat = Lacaml.D.Mat  


type mat = L.mat 

let cpy  x = 
  L.lacpy x 

let identity = Mat.identity 

let of_array = Mat.of_array 

let sub_matrix ~n ~m a = 
  L.lacpy ~n ~m a

let add x y = 
  L.gemm ~beta:1. ~c:(L.lacpy y) (Mat.identity (Mat.dim1 x)) x   
  (* very ineficient *)

let mul x y = 
  L.gemm x y 

let transpose x = 
  Mat.transpose x 

let eigen_decomposition ?n m = 
  let m' = cpy m in 
  let e  = L.syev ?n ~vectors:true m' in 
  (e, m')
  

let print name a =  
    Format.printf "@[<2>%s =@\n@\n@[%a@]@]@\n@\n%!" name Lacaml.Io.pp_fmat a

