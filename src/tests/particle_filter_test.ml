
let () = 
  Random.self_init (); 
  let gaussian = Gaussian1D.create ~mean:101.5 ~variance:10. in 
  let add_weight = List.map (fun x -> 
    (x, Gaussian1D.x gaussian (float_of_int @@ Char.code x))) 
  in 
  let p = ['a'; 'b'; 'c'; 'd'; 'e'; 'f' ; 'g'; 'h'; 'i'; 'j'; 'k'] in 
  let p = p @ p @ p @ p  in 
  let rec loop (acc: char list) = function 
    | 0 -> acc 
    | n -> (
      print_endline  @@ String.concat "," (List.map Char.escaped acc); 
      loop (Particle_filter.sample (add_weight acc)) (n - 1) 
    )
  in  
  ignore @@ loop p 50

