open Base
open Lamp
open Ast
open Eval

module Dsl = struct
  let lam x e = Lambda (Scope (x, e))
  let v x = Var x
  let i n = Num n
  let ( + ) e1 e2 = Binop (Add, e1, e2)
  let ( - ) e1 e2 = Binop (Sub, e1, e2)
  let ( * ) e1 e2 = Binop (Mul, e1, e2)
  let let_ x e1 ~in_:e2 = Let (e1, Scope (x, e2))
  let app e1 e2 = App (e1, e2)
end

(* Unit test utilities *)
let texpr = Alcotest.testable Pretty.pp_expr equal_expr
let tvars = Alcotest.testable Vars.pp Vars.equal
let parse = Parse_util.parse

(** Test free variable function *)
let test_free_vars (e : expr) (expected : string list) () =
  Alcotest.(check' tvars)
    ~msg:"same set" ~expected:(Vars.of_list expected) ~actual:(free_vars e)

(** Test free variable function with concrete syntax input *)
let test_free_vars_s (e_str : string) (expected : string list) () =
  test_free_vars (parse e_str) expected ()

(** Test substitution c[ x |-> e ] = expected *)
let test_subst ~(x : string) ~(e : expr) ~(c : expr) (expected : expr) () =
  let c' =
    try subst x e c with Stuck msg -> failwith ("Got stuck!\n" ^ msg)
  in
  Alcotest.(check' texpr) ~msg:"same expr" ~expected ~actual:c'

(** Test substitution c[ x |-> e ] = expected, with concrete syntax inputs *)
let test_subst_s ~(x : string) ~(e : string) ~(c : string) (expected : string)
    () =
  test_subst ~x ~e:(parse e) ~c:(parse c) (parse expected) ()

(** Check an expression evaluate to the expected value *)
let test_eval (e : expr) (expected : expr) () =
  let v = try eval e with Stuck msg -> failwith ("Got stuck!\n" ^ msg) in
  Alcotest.(check' texpr) ~msg:"eval" ~expected ~actual:v

(** Check a expression (concrete syntax) evaluate to the expected value (concrete syntax) *)
let test_eval_s (e_str : string) (expected_str : string) () =
  test_eval (parse e_str) (parse expected_str) ()

(** Check an expression gets stuck during evaluation *)
let test_stuck (e : expr) () =
  try
    let v = eval e in
    Alcotest.fail (Fmt.str "evaluated to %a" Pretty.pp_expr v)
  with Stuck _ -> ()

(** Check a expression (concrete syntax) gets stuck during evaluation *)
let test_stuck_s (e_str : string) = test_stuck (parse e_str)

let free_vars_tests = [ test_free_vars_s "lambda x. y" [ "y" ] ]

let subst_tests =
  [ test_subst_s ~x:"var" ~e:"1" ~c:"var + var" (*expected *) "1 + 1" ]

let eval_tests = [ test_eval_s (* input *) "1+2" (* expected *) "3" ]
let eval_stuck_tests = [ test_stuck_s (* input *) "(lambda x. x) + 1" ]

let subst_capture_tests =
  [
    test_subst_s ~x:"x" ~e:"lambda x. y" ~c:"lambda y. y x"
      (* expected output *) "lambda y0. (y0 (lambda x. y))";
  ]

let tests =
  [
    ("free_vars", free_vars_tests);
    ("subst", subst_tests);
    ("eval", eval_tests);
    ("eval_stuck", eval_stuck_tests);
    ("<bonus> subst_capture", subst_capture_tests);
  ]
