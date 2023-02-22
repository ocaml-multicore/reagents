let num_domains = 4

module Scheduler = (val Sched_ws.make num_domains ())
module Reagents = Reagents.Make (Scheduler)
open Reagents
module RLock = Sync.Recursive_lock (Scheduler)
module CDL = Sync.Countdown_latch

let rec lock_and_call l i =
  run (RLock.acq l) ();
  if i > 0 then lock_and_call l (i - 1);
  assert (run (RLock.rel l) ())

let test1 () =
  Scheduler.run (fun () ->
      let l = RLock.create () in

      Scheduler.fork (fun () ->
          run (RLock.acq l) ();
          lock_and_call l 1;
          ignore (run (RLock.rel l) ()));

      let cdl = CDL.create (100 + (100 * 2)) in
      (* ... *)
      for _ = 0 to 99 do
        Scheduler.fork (fun () ->
            lock_and_call l 100;
            run (CDL.count_down cdl) ())
      done;
      (* ... *)
      for _ = 0 to 99 do
        Scheduler.fork (fun () ->
            run (RLock.acq l) ();
            assert (run (RLock.try_acq l) ());
            assert (run (RLock.rel l) ());
            assert (run (RLock.rel l) ());
            run (CDL.count_down cdl) ());
        Scheduler.fork (fun () ->
            if run (RLock.try_acq l) () then (
              lock_and_call l 100;
              assert (run (RLock.rel l) ()));
            run (CDL.count_down cdl) ())
      done;
      run (CDL.await cdl) ())

let () =
  let open Alcotest in
  run "recursive lock test"
    [ ("simple", [ test_case "4-domain" `Quick test1 ]) ]
