module Scheduler = (val Sched_ws.make ~raise_if_all_idle:true 1 ())
module Reagents = Reagents.Make (Scheduler)
open Reagents

(* This file contains two tests which fail due to pair composition not executing in parallel.

   For example: A <*> B actually requires A to execute before B, and that cannot terminate
   if A depends on B. Note, the first test does not use <*> explicitely, but said composition
   still happens inside channel implementation.
*)

(* Test 1 *)
let mk_tw_chan () =
  let a_p, a_m = Channel.mk_chan ~name:"a" () in
  let b_p, b_m = Channel.mk_chan ~name:"b" () in
  ((a_p, b_p), (a_m, b_m))

let tw_swap (c1, c2) = Channel.swap c1 >>> Channel.swap c2

let work sw v () =
  let x = run (tw_swap sw) v in
  Printf.printf "%d" x

let two_way () =
  Scheduler.run_allow_deadlock (fun () ->
      let sw1, sw2 = mk_tw_chan () in
      Scheduler.fork (work sw1 1);
      work sw2 2 ())

(* Test 2 *)
let mk_tw_chan () =
  let ab, ba = Channel.mk_chan ~name:"ab" () in
  let bc, cb = Channel.mk_chan ~name:"bc" () in
  let ac, ca = Channel.mk_chan ~name:"ac" () in
  ((ab, ac), (ba, bc), (ca, cb))

let tw_swap (c1, c2) = Channel.swap c1 <*> Channel.swap c2

let work sw v () =
  let x, y = run (tw_swap sw) v in
  Printf.printf "%d %d" x y

let three_way () =
  Scheduler.run_allow_deadlock (fun () ->
      let sw1, sw2, sw3 = mk_tw_chan () in
      Scheduler.fork (work sw1 1);
      Scheduler.fork (work sw2 2);
      work sw3 3 ())

let () =
  let open Alcotest in
  run "paired composition not parallel"
    [
      ( "simple",
        [
          test_case "two-way swap" `Quick two_way;
          test_case "three-way swap" `Quick three_way;
        ] );
    ]
