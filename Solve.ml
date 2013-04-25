open List;;
open Num;;

type expr = 
  Value of num
| Sum of expr * expr
| Diff of expr * expr
| Prod of expr * expr
| Quot of expr * expr


exception DivisionByZero



(* -------------------- I/O utilities -------------------- *)
let rec output_expr ( oc : out_channel ) ( e : expr ) =
  match e with
    Value (Int v) -> output_string oc (" " ^ (string_of_int v))
  | Sum(a,b) -> output_string oc " (+"; output_expr oc a; output_expr oc b; output_string oc ")"
  | Diff(a,b) -> output_string oc " (-"; output_expr oc a; output_expr oc b; output_string oc ")"
  | Prod(a,b) -> output_string oc " (x"; output_expr oc a; output_expr oc b; output_string oc ")"
  | Quot(a,b) -> output_string oc " (/"; output_expr oc a; output_expr oc b; output_string oc ")"
  | _ -> output_string oc " ?"

let print_expr ( e : expr ) =
  output_expr stdout e; print_string "\n"

let print_expr_list ( elst : expr list ) =
  iter print_expr elst

let rec eval = function
  | Value (a) -> a
  | Sum (a,b) -> (eval a) +/ (eval b)
  | Diff (a,b) -> (eval a) -/ (eval b)
  | Prod (a,b) -> (eval a) */ (eval b)
  | Quot (a,b) -> let c = (eval b) in 
                  if 0 != (compare_num c (Int 0)) 
                  then (eval a) // c 
                  else raise DivisionByZero

let mappend ( fn : 'a -> 'b list ) ( lst : 'a list ) =
  fold_left (fun y x -> append (fn x) y) [] lst
  
(* pair_combinations: generates list of lists where each element list
   has the same length of the input list, and the first 2 elements of
   those lists forms the full 2-combinations set of the input list *)
let pair_combinations ( lst : 'a list ) =
  let rec split3 head rest accu =
    match rest with
      h::l -> split3 (h::head) l ((head,h,l)::accu)
    | _ -> accu
  and reassemble (head,mid,tail) =
    mid::(append tail head)
  in
  let rec shift_pair (head,mid,tail) =
    map (fun (h,m,t) -> mid::(append (reassemble (h,m,t)) head)) (split3 [] tail [])
  in mappend shift_pair (split3 [] lst [])

let create_solver ( target : int ) =
  let tar = Int target in
  let reduce_step = function
    | a::b::l -> [(Sum (a,b))::l;(Diff (a,b))::l;(Diff (b,a))::l;
                  (Prod (a,b))::l;(Quot (a,b))::l;(Quot (b,a))::l]
    | _ -> [] in
  let rec solve_iter ( remain : expr list ) =
    match remain with
      h::[] -> if (try 0 = (compare_num tar (eval h)) with DivisionByZero -> false )
        then [h] else []
    | h::l -> mappend solve_iter (mappend reduce_step (pair_combinations remain))
    | _ -> [] (* empty list *)
  in fun ( lst : int list ) -> solve_iter (map (fun x -> Value (Int x)) lst)

let solve24 ( lst : int list ) = create_solver 24 lst
        
        
        


        
        
        

        
        



