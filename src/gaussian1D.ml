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

let histogram_from_samples nb = function
  | [] -> failwith "Cannot create histogram from samples"
  | (x::samples) as all_samples -> 
    let min, max, n = List.fold_left (fun (min_, max_, n) x -> 
      (min x min_, max x max_, n+1)
    ) (x, x, 1) samples in 
    let dx  =  
      if min = max 
      then 1. 
      else (max +. (100. *. epsilon_float) -. min) /. (float_of_int nb) in 
    let bi  = fun x -> int_of_float @@ (x -. min) /. dx in 
    let xi  = 1. /. (float_of_int n *. dx) in 

    let h   = Array.make nb 0. in 
    List.iter (fun x -> 
      Array.set h (bi x) ((Array.get h (bi x))  +. xi) 
    ) all_samples; 

    let h = Util.fold_righti (fun i hv l -> 
      ((min +. (float_of_int i) *. dx  +. (dx /. 2.)), hv)::l 
    ) h [] in 
    h
    


