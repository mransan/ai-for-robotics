
module Mat = Matrix_util

let print_vec name a =  
    Format.printf "@[<2>%s =@\n@\n@[%a@]@]@\n@\n%!" name Lacaml.Io.pp_fvec a

let () = 

  let a = Mat.of_array [|
    [| 1. ;  1. |]; 
    [| 0. ; 2. |]; 
  |] in  

  let (e, a)  = Mat.eigen_decomposition a in 
  print_vec "e" e;
  Mat.print "a" a;
  ()
