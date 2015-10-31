(** Miscellanous utilities *)

val fold_n : ('a -> 'a) -> 'a -> int -> 'a 
(** [fold_n f e0 n] is [f1 (f2 (f3 .... (fn e0)))] *) 

val within_n_eps : ?n:int -> float -> float -> bool
(** [within_n_eps ~n a b] returns true if the difference between [a] and [b] is
    less then n times the floating point precision [Pervasives.epsilon_float]
 *)

val within : delta:float -> float -> float -> bool 
(** [within ~delta x y] returns true if the absolute difference of [x] and [y]
    is strictly less than [delta]. 
  *)
