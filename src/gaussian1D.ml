type t = { 
  mean : float; 
  variance : float; 
} 

let create ~mean ~variance = {mean; variance; }

let mean {mean; _ } = mean 

let variance {variance; _ } = variance 

let pi = 4. *. atan 1. 

let x {mean; variance; } x = 
  let den = sqrt @@ 2. *. pi *. variance in 
  let nom = exp @@  -. 0.5 *. ((x -. mean) ** 2.)/. variance in 
  nom /. den 

let update 
  {mean = mean1; variance = variance1} 
  {mean = mean2; variance = variance2} = {
    mean = (mean1 *. variance2 +. mean2 *. variance1) /. (variance1 +. variance2);
    variance = 1. /. ((1. /. variance1) +. (1. /. variance2)); 
  }

let predict 
  {mean = mean1; variance = variance1} 
  {mean = mean2; variance = variance2} = {
    mean = mean1 +. mean2; 
    variance = variance1 +. variance2; 
  }
