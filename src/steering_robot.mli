(** Steering Robot *) 

(** Steering robot configuration 
 *)
module Config : sig 

  type t 
  (** configuration type *)
  
  val create : ?distance_noise:float -> ?steering_noise:float -> ?bearing_noise:float -> length:float -> unit -> t   
  (** [create ~bearing_noise ~length] creates a new configuration *)

  val bearing_noise : t -> Gaussian1D.t 
  (** [bearing_noise config] returns the bearing noise value *)

end 

(** Robot motion 
 *) 
module Motion : sig 

  type t 
  (** motion type *)
  
  val create : steering:float -> distance:float -> t 
  (** [create ~steering ~distance] creates a new motion step *)
end 

module Landmark : sig 

  type t 
  (** Landmark type *)

  val create : x:float -> y:float -> t 
  (** [create ~x ~y] creates a new landmark *)
end 

(** Robot position 
 *)
module Pos : sig

  type t
  (** robot position *)

  val zero : t 
  (** [zero] is the location 0, 0, 0 *)
  
  val x : t -> float 
  (** [x pos] returns x corrdinate *)

  val y : t -> float 
  (** [y pos] returns y corrdinate *)

  val theta : t -> float 
  (** [theta pos] returns theta orientation in radians *)
  
  val abs_distance : ?from:t -> t -> float 
  (** [distance ~from pos] returns the distance between [pos] and [from]. 
      
      [from] defaults to [zero].
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
end 


val update_pos : ?with_noise:unit -> Config.t -> Pos.t -> Motion.t -> Pos.t

type bearing = float 

val bearing : ?with_noise:Config.t -> Pos.t -> Landmark.t -> bearing 
