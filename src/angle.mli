(** Angle functionality *)

val pi : float 
(** pi constant *)

val normalize : float -> float
(* [normalize angle] normalize [angle] between [0.] and [2 *. pi] value *)

val degrees_of_radians : float -> float 
(** [degrees_of_radians radian] convert the radian value to degrees *)
