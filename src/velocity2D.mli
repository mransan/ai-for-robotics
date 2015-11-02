(** Velocity module for 2D planar robots *)


type t = {
  v: float;
  w: float;
}
(** velocity type *)

val create : ?v:float -> ?w:float -> unit -> t 
(** [create ~v ~w] creates a new velocity *)

type noise 

val create_noise : float -> float -> float -> float -> noise
(** [create a1 a2 a3 a4] creates a new noise definition *)

type dt = float 
(** Timestamp type *)

val noise_matrix : t -> noise -> Lacaml_D.mat 
(** [noise_matrix velocity noise] returns the noise matrix 
    as defined in Equation 7.10
  *)

val update_pos : t -> float -> Pos2D.t -> Pos2D.t 
(** [update_pos velocity dt pos] returns the new position of the 
    robot after [dt] assuming constant [velocity].
 *)

val pos_jacobian : t -> dt -> Pos2D.t -> Lacaml_D.mat 
(** [pos_jacobian velocity dt pos] returns the Jacobian matrix 
    as described at 7.8 & 7.9
 *)

val motion_jacobian : t -> dt -> Pos2D.t -> Lacaml_D.mat 
(** [motion_jacobian velocity dt pos] returs the new motion 
    Jacobian matrix as described in 7.11
  *) 

val kf: t -> noise -> dt -> Pos2D.t -> Lacaml_D.mat -> (Pos2D.t * Lacaml_D.mat)


