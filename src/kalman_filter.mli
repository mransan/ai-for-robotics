(** Kalman filter *) 

(** Some notation information
    
    We use the following naming convention : 
    {ul 
    {- 'x' refers to the state }
    {- 'z' refers to the measurement }
    {- 'u' refers to the control }
    }
    
    Therefore we have the following names 
    {ul
    {- {e x}: state vector (mean of the state guassian distribution) } 
    {- {e x_cov}: state covariance matrix } 
    {- {e x_f}: state transition function (x -> x)}   
    {- {e x_d}: state dimension }
    {- {e u}: the control (or force) vector}
    {- {e u_f}: the control function (u -> x) }   
    {- {e u_d}: control dimension } 
    {- {e z}: the measurement vector }
    {- {e z_cov}: measurement covariance matrix} 
    {- {e z_f}: measurement function (x -> z) }
    {- {e z_d}: measurement dimension}
    }
*)

type t 
(** kalman filter configuration *)

type dim = int * int 
(** dimension type [(state_dimenstion, measurement_dimension)] *)

val create : 
  dim:dim -> 
  x_f:Lacaml.D.mat -> 
  z_f:Lacaml.D.mat -> 
  z_cov:Lacaml.D.mat -> 
  t
(** [create ~dim ~state_f ~measurement_f ~measurement_uncertainty] 
    creates a new kalman filter 

    [state_f] is a square matrix of [state_dimension] size. 
    [measurement_f] is a [measurement_dimension] x [state_dimension] size. 
    [measurement_uncertainty] is a square matrix of [measurement_dimension]
*) 

val update : ?x_f:Lacaml.D.mat -> ?z_f:Lacaml.D.mat -> ?z_cov:Lacaml.D.mat -> t -> t 
(** [update ~x_f ~z_f ~z_cov kf] will create a new kalman filter based on [kf]
    with [x_f], [z_f], [z_cov] updated 

    @raises Failure in case the matrix size are not of the Kalman filter initial
    dimension.
 *)
 
val predict : t -> x:Lacaml.D.mat -> x_cov:Lacaml.D.mat -> (Lacaml.D.mat * Lacaml.D.mat) 
(** [predict kf ~x ~p] returns the updated state value [x] and 
    state uncertainty [p] 
    
    [x] and [p] are not modified ... new matrix values are returned.
 *) 

val add_measurement : t -> z:Lacaml.D.mat -> x:Lacaml.D.mat -> x_cov:Lacaml.D.mat -> (Lacaml.D.mat * Lacaml.D.mat)
(** [add_measurement kf ~z ~x ~p] update state value [x] and uncertainty [p]  with the measurement
    [z].

    [x] and [p] are not modified ... new matrix values are returned. 
 *)
