(** Angle functionality *)

val pi : float 
(** pi constant *)

val normalize : ?type_:[< `Zero_2_pi | `Zero_centered > `Zero_2_pi ] -> float -> float
(* [normalize ~type_ angle] normalize [angle] using the [type_] range: 
   {ul
   {- [Zero_2_pi] : the normalized angle will be between [0.] and [2. *. pi] }
   {- [Zero_centered] : the normalized angle will be between [-. pi] and [pi] }
   }
 *)

val degrees_of_radians : float -> float 
(** [degrees_of_radians radian] convert the radian value to degrees *)

(* TODO add radians_of_degrees *)
