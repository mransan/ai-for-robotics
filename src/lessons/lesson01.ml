type world = {
  x : float array; 
  s : [`Red | `Green] array;
}
(** world type *) 

let create_world type_ s = 
  let n = Array.length s in 
  match type_ with 
  | `Uniform ->  { 
    x = Array.make n (1. /. (float_of_int n)); 
    s; 
  } 
  | `At i -> (
    let world = { 
      x = Array.make n 0.; 
      s; 
    } in 
    Array.set world.x i 1.; 
    world 
  )
(** create a new world with 2 possible setup: either the position is 
    known 100% or the location is completely unknown.
 *) 

let move_proba = [ (-1, 0.1); (0 , 0.8); (1 , 0.1); ] 
(** The probability distribution for moving the robot. *)

let move_by_n world n =  
  let world_size = Array.length world.x in 
  let x = Array.mapi (fun i _ -> 
    List.fold_left (fun p (offset, offset_p) -> 
      let i' = i - n  + offset + world_size in 
      let i' = i' mod world_size in 
      p +. offset_p *. (Array.get world.x i') 
    ) 0. move_proba 
  ) world.x in 
  { world with x }  
(** [move_by_n world distance] moves the robot by [distance] and returns 
    a brand new world.
  *)

(* p(Correct   | Assuming you are at the location) = 0.6 
   p(Incorrect | Assuming you are at the incorrect location) = 0.2  
 *)

let string_of_world world = 
  String.concat "|" @@ Array.to_list (Array.map (fun x ->
    Printf.sprintf "%4.2f" x
  ) world.x) 
(** [string_of_world world] returns a debugging string *)

let sense world which = 
  let x = Array.mapi (fun i p -> 
      let truth = Array.get world.s i in 
      if truth = which
      then 0.6 *. p 
      else 0.2 *. p  
  ) world.x in 
  let sum = Array.fold_left (fun s p -> s +. p) 0. x in 
  {world with x = Array.map (fun p -> p /. sum) x  }  

(* Test case 1: 
  
   In this test case we start with the robot at a known location and we make it
   move n time. If n is large we eventually arrive in a world where the position 
   of the robot is a uniform distribution. 
 *)
let () = 
  let rec loop world = function 
    | 0 -> () 
    | i -> (
      print_endline @@ string_of_world  world;  
      loop (move_by_n world 1) (i - 1)
    ) 
  in 
  loop (create_world (`At 0) [| `Green; `Red; `Red; `Green; `Green; `Green |] ) 100

(* Test case 2 
 
   In this test case we will start with a uniform distribution of the robot 
   location (ie no clue where it is) and we will keep observing a unique 
   location. 

   Eventually we should be 100% sure that the robot is in the repeatidly 
   observed location.
 *)
let () = 

  let rec loop world = function 
    | 0 -> () 
    | i -> (
      print_endline @@ string_of_world  world;  
      loop (sense world `Red) (i - 1)
    ) 
  in 
  loop (create_world `Uniform [| `Green; `Green;  `Red; `Green; `Green |]) 100

