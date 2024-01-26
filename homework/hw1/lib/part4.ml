open Base
open Util

type expr =
  | Const of int
  | X
  | Add of expr * expr
  | Mul of expr * expr
  | Compose of expr * expr

(* Pretty-printer *)
let rec pp_expr ppf = function
  | Const n -> Fmt.pf ppf "%d" n
  | X -> Fmt.pf ppf "x"
  | Add (e1, e2) -> Fmt.pf ppf "(%a + %a)" pp_expr e1 pp_expr e2
  | Mul (e1, e2) -> Fmt.pf ppf "(%a * %a)" pp_expr e1 pp_expr e2
  | Compose (e1, e2) -> Fmt.pf ppf "(%a; %a)" pp_expr e1 pp_expr e2

(* Convert an expression into a pretty string *)
let show_expr (e : expr) : string = Fmt.to_to_string pp_expr e
let rec eval_expr (x : int) (e : expr) : int = 
  match e with
  | X -> x
  | Const i -> i
  | Add (i,j) -> eval_expr x i + eval_expr x j
  | Mul (i,j) -> eval_expr x i * eval_expr x j
  | Compose (i,j) -> eval_expr (eval_expr x i) j

(* x+1;(2*x; x+x) *)
 (* | Compose(i, j) -> 
      let no_comp_i = remove_comp i in
      let no_comp_j = remove_comp j in 
      (match (no_comp_i, no_comp_j) with
        | _, Add(Const a,X) -> Add(Const a, no_comp_i)
        | _, Add(X,Const b) -> Add(no_comp_i, Const b)
        | _, Add(X,X) -> Add(no_comp_i, no_comp_i)
        | _, Mul (Const a, X) -> Mul(Const a, no_comp_i)
        | _, Mul (X, Const b) -> Mul(no_comp_i, Const b)
        | _, Mul (X, X) -> Mul(no_comp_i, no_comp_i)
        | 
        | _, _ -> no_comp_j) *)

let rec remove_nested e sub = 
  match e with
  | Const a -> e
  | X -> sub
  | Add(i, j) -> Add(remove_nested i sub, remove_nested j sub)
  | Mul(i, j) -> Mul(remove_nested i sub, remove_nested j sub)
  | Compose (i, j) -> Compose (remove_nested i sub, remove_nested j sub)

let rec remove_comp (e:expr) : expr = 
  match e with
  | Add (i,j) -> Add (remove_comp i, remove_comp j)
  | Mul (i,j) -> Mul (remove_comp i, remove_comp j)
  | Compose (i, j) -> 
    let no_comp_i = remove_comp i in 
      remove_comp (remove_nested j i)
  | _ -> e


  let rec simplify (e : expr) : expr =
    let no_comp_e = remove_comp e in 
    match no_comp_e with
    | Add (i, j) ->
      let simplified_i = simplify i in
      let simplified_j = simplify j in
      (match (simplified_i, simplified_j) with
       | Const a, Const b -> Const (a + b)
       | _, Const 0 -> simplified_i
       | Const 0, _ -> simplified_j
       | _ -> Add (simplified_i, simplified_j))
  
    | Mul (i, j) ->
      let simplified_i = simplify i in
      let simplified_j = simplify j in
      (match (simplified_i, simplified_j) with
       | Const a, Const b -> Const (a * b)
       | _, Const 0 -> Const 0
       | Const 0, _ -> Const 0
       | Const 1, _ -> simplified_j
       | _, Const 1 -> simplified_i
       | _ -> Mul (simplified_i, simplified_j))

    | _ -> no_comp_e

type poly = int list [@@deriving show]

let rec eval_poly (x : int) (p : poly) : int = 
  match p with
  | [] -> 0
  | [i] -> i
  | h :: t -> h + (x * eval_poly x t)


let rec normalize (e : expr) : poly = bonus ()
let semantic_equiv (e1 : expr) (e2 : expr) : bool = bonus ()
