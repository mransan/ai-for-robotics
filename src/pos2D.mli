(** 2D position for a planar robot *)

type t = {
  x: float; 
  y: float; 
  theta: float; 
}
(** robot position *)

val zero : t 
(** [zero] is the location 0, 0, 0 *)

val x : t -> float 
(** [x pos] returns x corrdinate *)

val y : t -> float 
(** [y pos] returns y corrdinate *)

val theta : t -> float 
(** [theta pos] returns theta orientation in radians *)

val distance : ?from:t -> t -> float 
(** [distance ~from pos] returns the distance between [pos] and [from]. 
    
    [from] defaults to [zero].
  *)

val bearing  : ?from:t -> t -> float 
(** [bearing ~from pos] returns the bearing in radian between [from] to 
    [pos].
 *)

val create : x:float -> y:float -> theta:float -> t
(** [create ~x ~y ~theta] create a new robot position *)

val average : t list -> t 
(** [average pos] return the average position *)

val to_string : ?excel:unit -> t -> string
(** [to_string pos] return a debuggint string for the position. 
    
    If [~excel:()] is provided then the string will compatible for 
    excel (comma separated format). 
  *)
