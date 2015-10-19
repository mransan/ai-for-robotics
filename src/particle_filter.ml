
(** Circular list 
 
    This module provides functionality around circular list. 
 *)
module Circular_list  = struct 

  type 'a t = { l: 'a list; sub: 'a list } 

  let create l = { l ; sub = l } 

  (** [until predicate e0 cl] will iterate over elements [e] of [cl] 
      until [predicate acc e] returns [(true, _)]. 
  
      The function returns [(acc, cl')] where [acc] is the accumulated value
      returned last by [predicate] and [sub] is the new sub list which head is the element [e] for
      which [predicate] returned true. 
  
      Note if [sub] is a sub list of [l] then this function is looping over a
      list until [predicate] returns true. 
  
      Note if [f] never returns [(true, _)] then this [until] never terminates. 
   *) 
  let until (p:('a -> 'b -> bool*'a))  (e0:'a) ({l ; sub }:'b t)  = 
    let rec loop acc = function
      | [] -> 
        loop acc l 
      | hd::tl -> 
        let done_, acc = p acc hd in
        if done_ then (acc, hd, {l; sub = hd::tl}) else loop acc tl 
    in 
    loop e0 sub 

end 


let sample  ?max population = 

  match population with 
  | []     -> [] 
  | hd::tl -> ( 
    let max = match max with 
      | Some v -> v 
      | None -> 
        List.fold_left (fun acc (_, (w:float)) -> 
          Pervasives.max acc w
        ) (snd hd) tl
    in 

    let increment_upper_bound = 2. *. max in 

    let rec loop p' circular_list beta = function
      | 0 -> p' 
      | n -> (
        let beta = beta +. Random.float increment_upper_bound in 
        let beta, (x, _), circular_list = Circular_list.until (fun beta (x, weight) -> 
          if beta < weight
          then (true, beta) 
          else (false, beta -. weight)
        ) beta circular_list
        in 
        loop (x::p') circular_list beta (n - 1) 
      ) 
    in 
    loop [] (Circular_list.create population) 0. (List.length population)  
  )
  
