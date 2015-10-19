(** Particle Filter routines *)

val sample : ?max:float -> ('a * float) list -> 'a list  
(** [sample population] samples the [population] according to each member weights *)
