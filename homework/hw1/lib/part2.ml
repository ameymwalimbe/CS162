open Base
open Util

let rec compress (equal : 'a -> 'a -> bool) (xs : 'a list) : 'a list = 
  match xs with
  | [] -> []
  | [x] -> [x]
  | x::y::tail -> 
    if equal x y then compress equal (y::tail) else x::compress equal (y::tail)


let max (xs : int list) : int option = 
  match xs with
  | [] -> None
  | h::t -> let rec max_helper el lst = 
    match lst with
    | [] -> el
    | hd :: tl -> if hd > el then max_helper hd tl else max_helper el tl
  in Some (max_helper h t)


let rec join (xs : 'a option list) : 'a list option = 
  match xs with 
  | [] -> Some []
  | None :: _ -> None
  | Some h :: t ->
    match join t with 
    | Some xlst -> Some (h::xlst)
    | None -> None

    (*
       Some 1, Some 2, Some 3, Some 4

       Some 1 :: t  => Some 1 , Some 2, Some 3, Some 4 , Some []
       join t
        Some 2 :: t => x , Some 2, Some 3, Some 4 , Some []
        join t
          Some 3 :: t  => x , Some 3, Some 4 , Some []
          join t
            Some 4:: t  => x , Some 4 , Some []
            join t
            [] -> Some []
    *)


let insert (k : 'k) (v : 'v) (d : ('k * 'v) list) : ('k * 'v) list = (k, v) :: d

let rec lookup (equal : 'k -> 'k -> bool) (k : 'k) (d : ('k * 'v) list) :
    'v option =
    match d with 
    | [] -> None
    | (x, y) :: t -> if equal k x then Some y else lookup equal k t
