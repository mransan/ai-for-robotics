type t 
(** 1 Dimensional Gaussian distribution *)

val create : mean:float -> variance:float -> t 
(** [create ~mean ~variance ] creates a new gaussian distribution *) 

val mean : t -> float 
(** [mean gaussian] returns the mean of the gaussian *)

val variance : t -> float 
(** [variance gaussian] returns the variance of the gaussian *)

val x : t -> float -> float 
(** [x gaussian] returns the gaussian function value *)

val update : t -> t -> t
(** [update g1 g2] returns the new updated g based on [g1] and [g2] *) 

val predict : t -> t -> t
(** [add g1 g2] returns the new updated g based on [g1] and [g2] *) 

val random : t -> float 
