type t = { 
  mean : float; 
  variance : float; 
  mutable previous_random : float option;
} 

let create ~mean ~variance = {mean; variance; previous_random = None}

let mean {mean; _ } = mean 

let variance {variance; _ } = variance 

let pi = 4. *. atan 1. 

let x {mean; variance; } x = 
  let den = sqrt @@ 2. *. pi *. variance in 
  let nom = exp @@  -. 0.5 *. ((x -. mean) ** 2.)/. variance in 
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
  | Some x -> (
    t.previous_random <- None; 
    x 
  )
  | None -> (
    let u1 = Random.float 1. in 
    let u2 = Random.float 2. in 
    let z0 = sqrt (m2 *. (log u1)) *. cos (2. *. pi *. u2) in  
    let z1 = sqrt (m2 *. (log u1)) *. sin (2. *. pi *. u2) in  
    t.previous_random <- Some z1; 
    z0 
  )
