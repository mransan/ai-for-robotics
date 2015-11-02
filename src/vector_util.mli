(** Vector Utilities *)

(** This module provides the basic functionality needed for the 
    robotic algorithm. It currently is implemented in terms 
    of the Lacaml library based on Lapack. 
    
    This is not guaranteed to be forever the case. This module therefore
    serves as the abstraction layer between the 2 and represent the minimal
    feature set required. 
  *)


(** {2 Types} *) 

type vec = Lacaml_D.vec 
(** vector type *)

(** {2 Utilities } *) 

val to_array: vec -> float array 
(** [to_array vec] converts [vec] to an OCaml float array *)

val print : string -> vec -> unit 
(** [print name vec] prints a debugging infor for [vec] *)
