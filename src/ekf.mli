(** Extended Kalman Filter *) 

(** This module implement the prediction/correction 
    algorithm of the Extended Kalkman filter. 
 *) 

(** State `x` signature.*)
module type X_sig = sig
  type x 
  (** State type *) 

  val mat_of_x : x -> Matrix_util.mat
  (** [mat_of_x x] returns the state vector in matrix format *)
  
  val x_of_mat : Matrix_util.mat -> x 
  (** [x_of_mat m] converts the state matrix to the state type *)
end 

module type U_sig = sig 
  type x 
  (** State type *) 

  type u 
  (** Command type *)

  val  g : x -> u -> x 
  (** [g x u] is the belief update function which computes 
      the posterior believe from the current belief and the 
      command. 
   *)

  val jacobian : u -> x -> Matrix_util.mat 
  (** [jacobian u x] returns the Jacobian of [g] for [u] and [x]
    *)

  val r: u -> x -> Matrix_util.mat 
  (** [r u x] returns the noise matrix associated with that 
      command. This noise will be added to the state 
      covariance matrix durng the kalman filter 
      predict step.   

      Matrix should be square and of same dimension as 
      the state covariance matrix.
   *) 

end

(** Measurement module *)
module type Z_sig = sig 
  type x 
  (** state type *)
  
  type z
  (** measurement type. Typically this type should be 
      made of both the definition of what is being measured 
      as well as the actuall measurement value 
    *)

  val mat_of_z : z -> Matrix_util.mat 
  (** [mat_of_z z] converts to a matrix *) 

  val h : z -> x -> z 
  (** [h z x] estimate from belief function. This function
      should return the estimated measurement from  
      the current state belief.    
   *) 

  val jacobian : z -> x -> Matrix_util.mat 
  (** [jacobian z x] returns the jacobian of [h] *)
  
  val q : z -> x -> Matrix_util.mat 
  (** [q z x] returns the noise matrix in the measurement space *)
end 

module Make (X:X_sig) 
            (U:U_sig with type x = X.x) 
            (Z:Z_sig with type x = X.x) : sig 
  
  val predict : X.x -> Matrix_util.mat -> U.u -> (X.x * Matrix_util.mat)
  (** [predict x s u] returns [(x, s)] the updated state [x] and covariance 
      matrix [s] 
   *)
  
  val correct : ?debug:unit -> X.x -> Matrix_util.mat -> Z.z -> (X.x * Matrix_util.mat)
  (** [correct x s z] applies the measurement correction using measurement [z]
      to state [x] with covariance [s].
   *)
end 
