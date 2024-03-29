open List;;
open Solve;;


exception OverflowError




let generate_sequence ( max : int ) ( len : int ) =
  let rec init l = if 0 = l then [] else 1::(init (l-1)) in
  let rec next ( x : int list ) = 
    match x with
      a::b -> if a < max then (a+1)::b else 1::(next b)
    | _ -> raise OverflowError in
  let rec gen_iter x accu =
       try let c = next x in
           gen_iter c (x::accu)
       with OverflowError -> accu 
  in gen_iter (init len) []
         
      
  
    
let test_success ( lst : int list ) = 
  if 0 = length (solve24 lst) then 0 else 1


let batch_test seq = 
  map test_success seq

let rec sum ( lst : int list ) = 
  fold_left (fun y x -> x + y) 0 lst
