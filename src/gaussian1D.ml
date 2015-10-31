type t = { 
  mean : float; 
  variance : float; 
  mutable previous_random : float option;
} 

let create ~mean ~variance = {mean; variance; previous_random = None}

let from_samples l = 
  let f (x, x2, n) x' = 
    x +. x', (x2 +. x'*.x'), n + 1 
  in 
  let x, x2, n = List.fold_left f (0., 0., 0) l in 
  let n = float_of_int n in 
  let variance = (x2 -. (x *. x /. n)) /. (n -. 1.) in  
  let mean = x /. n in 
  create ~mean ~variance 

let mean {mean; _ } = mean 

let variance {variance; _ } = variance 

let pi = 4. *. atan 1. 

let x {mean; variance; } x = 
  let den = sqrt (2. *. pi *. variance) in 
  let nom = exp  (-. 0.5 *. ((x -. mean) ** 2.) /. variance) in 
  nom /. den 

let update 
  {mean = mean1; variance = variance1; _ } 
  {mean = mean2; variance = variance2; _ } = {
    mean = (mean1 *. variance2 +. mean2 *. variance1) /. (variance1 +. variance2);
    variance = 1. /. ((1. /. variance1) +. (1. /. variance2)); 
    previous_random = None; 
  }

let predict 
  {mean = mean1; variance = variance1; _ } 
  {mean = mean2; variance = variance2; _ } = {
    mean = mean1 +. mean2; 
    variance = variance1 +. variance2; 
    previous_random  = None; 
  }

let m2 = -. 2. 

let random ({mean; variance; previous_random} as t) = 
  match previous_random with 
  | Some z1 -> (
    t.previous_random <- None; 
    mean +. z1 *. (sqrt variance) 
  )
  | None -> (
    let u1 = Random.float 1. in 
    let u2 = Random.float 1. in 
    let z0 = (sqrt (m2 *. (log u1)) ) *. cos (2. *. pi *. u2) in  
    let z1 = (sqrt (m2 *. (log u1)) ) *. sin (2. *. pi *. u2) in  
    t.previous_random <- Some z1; 
    mean +. z0 *. (sqrt variance)
  )
