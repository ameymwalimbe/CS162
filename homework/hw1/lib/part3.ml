open Base
open Util

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree [@@deriving show]

let rec equal_tree (equal : 'a -> 'a -> bool) (t1 : 'a tree) (t2 : 'a tree) :
    bool =
    match t1, t2 with
    | Leaf, Leaf -> true
    | Node(p1,c1, c2), Node(p2, c3, c4) -> if equal p1 p2 then equal_tree equal c1 c3 && equal_tree equal c2 c4 else false
    | _ -> false 


let rec ts_helper count tr  = 
  match tr with 
  | Leaf -> (Leaf), count
  | Node (p, left, right) -> 
    let left_tree, next_count = ts_helper (count+1) left in 
    let right_tree, next_next_count = ts_helper next_count right in 
    (Node((count, p), left_tree, right_tree), next_next_count)
        
let timestamp (t : 'a tree) : (int * 'a) tree = 
    match ts_helper 0 t with
    (a,b) -> a

(*
(Leaves not shown)
       'o'                          (0,'o')
      /   \                        /       \
   'm'     'y'     =>       (1,'m')        (4,'y')
   / \     / \              /     \        /     \
 'c' 'a' 'm' 'l'      (2,'c') (3,'a')   (5,'m')  (6,'l')


       'o'                         
      /   \                        
   'm'     'y'

   1) ts_helper 0 Node(o, m, y)

   2)  left_tree, next_count = ts_helper 0+1 m
        ts_helper 1 m returns (1, m), 2
        so
        left_tree, next_count = 2, (1,m)

      right_tree, next_next_count = ts_helper next_count(2) y
        ts_helper 2 y returns (2,y), 3
        so 
        right_tree, next_next_count = (2,y), 3

      Node ((0, a), left_tree, right_tree)
*)



