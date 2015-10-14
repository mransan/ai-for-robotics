



type t 
(** kalman filter configuration *)

type dim = int * int 
(** dimension type [(state_dimenstion, measurement_dimension)] *)

val create : 
  dim:dim -> 
  state_f:Lacaml.D.mat -> 
  measurement_f:Lacaml.D.mat -> 
  measurement_uncertainty:Lacaml.D.mat -> 
  t
(** [create ~dim ~state_f ~measurement_f ~measurement_uncertainty] 
    creates a new kalman filter 

    [state_f] is a square matrix of [state_dimension] size. 
    [measurement_f] is a [measurement_dimension] x [state_dimension] size. 
    [measurement_uncertainty] is a square matrix of [measurement_dimension]
*) 
 
val predict : ?u:Lacaml.D.mat -> t -> x:Lacaml.D.mat -> p:Lacaml.D.mat -> (Lacaml.D.mat * Lacaml.D.mat) 
(** [predict kf ~x ~p] returns the updated state value [x] and 
    state uncertainty [p] 
    
    [x] and [p] are not modified ... new matrix values are returned.
 *) 

val add_measurement : t -> z:Lacaml.D.mat -> x:Lacaml.D.mat -> p:Lacaml.D.mat -> (Lacaml.D.mat * Lacaml.D.mat)
(** [add_measurement kf ~z ~x ~p] update state value [x] and uncertainty [p]  with the measurement
    [z].

    [x] and [p] are not modified ... new matrix values are returned. 
 *)

