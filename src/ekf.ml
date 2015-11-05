module type X_sig = sig
  type x 

  val mat_of_x : x -> Matrix_util.mat
  
  val x_of_mat : Matrix_util.mat -> x 
end 

module type U_sig = sig 
  type x 

  type u 

  val  g : x -> u -> x 

  val jacobian : u -> x -> Matrix_util.mat 

  val r: u -> x -> Matrix_util.mat 
end


module type Z_sig = sig 
  type x 
  
  type z
  
  val mat_of_z : z -> Matrix_util.mat 

  val h : z -> x -> z 

  val jacobian : z -> x -> Matrix_util.mat 
  
  val q : z -> x -> Matrix_util.mat 
end 

module Make (X:X_sig) 
            (U:U_sig with type x = X.x) 
            (Z:Z_sig with type x = X.x) = struct 
  
  let predict x s u = 
    let open Matrix_util.Ops in 
    let gm = U.jacobian u x in 
    let r  = U.r u x in 
    let x' = U.g x u  in 
    let s' = gm *~ s *~ (Matrix_util.transpose gm) +~ r in 
    (x', s')  

  let correct x s z = 
    let open Matrix_util.Ops in 
    let zm  = Z.mat_of_z z in       (* measure *) 
    let zpm = Z.mat_of_z (Z.h z x) in (* from belief *)
    let hm  = Z.jacobian z x in 
    let si  = hm *~ s *~ (Matrix_util.transpose hm) +~  (Z.q z x) in
    Lacaml_D.getri si; 
    let ki  = s *~ (Matrix_util.transpose hm) *~ si in 
    let x   = (X.mat_of_x x) +~ ki *~ (zm -~ zpm) in
    let s   = (Matrix_util.identity (Matrix_util.square_dim s) -~ ki *~ hm) *~ s in 
    (X.x_of_mat x, s)  
end 
