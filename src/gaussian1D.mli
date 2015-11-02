type t 

(** 1 Dimensional Gaussian distribution *)

(** {2 Creation} *) 

val create : mean:float -> variance:float -> t 
(** [create ~mean ~variance ] creates a new gaussian distribution *) 

val from_samples : float list -> t 
(** [from_samples samples] returns the gaussian distribution of the given sample
 *) 

(** {2 Accessors} *)

val mean : t -> float 
(** [mean gaussian] returns the mean of the gaussian *)

val variance : t -> float 
(** [variance gaussian] returns the variance of the gaussian *)

val x : t -> float -> float 
(** [x gaussian] returns the gaussian function value *)

(** {2 Computation} *) 

val update : t -> t -> t
(** [update g1 g2] returns the new updated g based on [g1] and [g2] *) 

val predict : t -> t -> t
(** [add g1 g2] returns the new updated g based on [g1] and [g2] *) 

val random : t -> float 
(** [random gaussian] generates a new random number. This random number is 
    using box muller method and depends on the [Random.float] uniform random number
    generator. All initialization logic is left to the client application.
 *)

(** {2 Utilities} *) 

val histogram_from_samples : int -> float list -> (float * float) list 
(** [histogram_from_samples nb_of_bucket samples] creates a histogram 
    of the sample distribution based on the number of bucket. 
 *) 
