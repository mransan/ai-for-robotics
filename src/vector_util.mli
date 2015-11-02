(** Vector Utilities *)


(** {2 Types} *) 

type vec = Lacaml_D.vec 
(** vector type *)


(** {2 Utilities } *) 

val to_array: vec -> float array 
(** [to_array vec] converts [vec] to an OCaml float array *)

val print : string -> vec -> unit 
(** [print name vec] prints a debugging infor for [vec] *)
