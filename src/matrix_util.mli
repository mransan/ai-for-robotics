(** Matrix utilities *)

(** This module provides the basic functionality needed for the 
    robotic algorithm. It currently is implemented in terms 
    of the Lacaml library based on Lapack. 
    
    This is not guaranteed to be forever the case. This module therefore
    serves as the abstraction layer between the 2 and represent the minimal
    feature set required. 
  *)

(** {2 Types} *) 

type mat = Lacaml.D.mat 
(** matrix type *)

(** {2 Creation} *) 

val identity : int -> mat 
(** [identity n] creates the identity matrix of size n x n *)

val of_array : float array array -> mat  
(** [of_array a] creates a matrix from [a] *)

val cpy : mat -> mat 
(** [cpy m] creates a copy of [m] *) 

val sub_matrix : n:int -> m:int -> mat -> mat 
(** [sub_matrix] extracts the top left sub matrix of size [n] x [m] *)

(** {2 Operation } *) 

val add : mat -> mat -> mat 
(**  [add x y] adds [x] and [y]. @raise Failure if [x] and [y] have different
     sizes *)

val mul : mat -> mat -> mat 
(**  [mul x y] multiplies [x] with [y]. 
     
     @raise Failure if [col x] is different than [row y] 
 *)

val transpose : mat -> mat 
(** [transpose m] transposes m *)

val eigen_decomposition : ?n:int -> mat -> (Vector_util.vec * mat) 
(** [eigen_decomposition m] returns the [(values, vectors]) where 
    [values] are the eigen values, and [vectors] the eigen vectors. 
 *)

(** {2 Conversion} *)

val to_array : mat -> float array array 
(** [to_array m] convert [mat] to an array. *)

val print : string -> mat -> unit 
(** [print name m] prints a debugging statement for [m] *)
