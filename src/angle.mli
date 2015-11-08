(** Angle functionality *)

val pi : float 
(** pi constant *)

val normalize : ?type_:[< `Zero_2_pi | `Zero_centered > `Zero_2_pi ] -> float -> float
(* [normalize angle] normalize [angle] between [0.] and [2 *. pi] value 
   TODO: add several normalizer method: 
   a) -pi/pi 
   b) -pi/pi centered around a different value than 0. 
 *)

val degrees_of_radians : float -> float 
(** [degrees_of_radians radian] convert the radian value to degrees *)

(* TODO add radians_of_degrees *)
