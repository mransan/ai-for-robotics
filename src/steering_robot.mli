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

val update_pos : ?with_noise:unit -> Config.t -> Pos2D.t -> Motion.t -> Pos2D.t

type bearing = float 

type landmark = Pos2D.t 

val bearing : ?with_noise:Config.t -> Pos2D.t -> landmark -> bearing 
