module Scheduler = (val Reagents.Toy_scheduler.make 1 ())
module Reagents = Reagents.Make (Scheduler)
open Reagents

let n = 20

let rec mk_answer acc = function
  | 0 -> acc
  | n -> mk_answer (Random.bool () :: acc) (n - 1)

let answer = mk_answer [] n

let rec make acc = function
  | 0 -> acc
  | n ->
      let r = constant true <+> constant false in
      make (r :: acc) (n - 1)

let rec join acc = function
  | [] -> constant (List.rev acc)
  | x :: xs -> x >>= fun v -> join (v :: acc) xs

let join l = join [] l

let test1 () =
  Scheduler.run (fun () ->
      let r =
        join (make [] n) >>= fun l ->
        (* instead of l = answer, assume `eval_formula l formula` where
           eval_formula : input:bool list -> formula -> bool *)
        if l = answer then (
          Printf.printf "SAT\n%!";
          constant ())
        else never
      in
      run r ())

let () =
  let open Alcotest in
  run "sat test" [ ("simple", [ test_case "test 1" `Quick test1 ]) ]
