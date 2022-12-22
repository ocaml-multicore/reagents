let num_domains = 4

module Scheduler = Sched_ws.Make (struct
  let num_domains = num_domains
  let is_affine = false
  let work_stealing = true
end)

module Reagents = Reagents.Make (Scheduler)
open Reagents
module Sync = Reagents.Sync
module RLock = Sync.Recursive_lock (Scheduler)
module CDL = Sync.Countdown_latch

let rec lock_and_call l i =
  run (RLock.acq l) ();
  if i > 0 then lock_and_call l (i - 1);
  assert (run (RLock.rel l) ())

let main () =
  let l = RLock.create () in

  Scheduler.fork_on
    (fun () ->
      run (RLock.acq l) ();
      lock_and_call l 1;
      ignore (run (RLock.rel l) ()))
    (2 mod num_domains);

  let cdl = CDL.create (100 + (100 * 2)) in
  (* ... *)
  for i = 0 to 99 do
    Scheduler.fork_on
      (fun () ->
        lock_and_call l 100;
        run (CDL.count_down cdl) ())
      (i mod num_domains)
  done;
  (* ... *)
  for i = 0 to 99 do
    Scheduler.fork_on
      (fun () ->
        run (RLock.acq l) ();
        assert (run (RLock.try_acq l) ());
        assert (run (RLock.rel l) ());
        assert (run (RLock.rel l) ());
        run (CDL.count_down cdl) ())
      (i mod num_domains);
    Scheduler.fork_on
      (fun () ->
        if run (RLock.try_acq l) () then (
          lock_and_call l 100;
          assert (run (RLock.rel l) ()));
        run (CDL.count_down cdl) ())
      (i mod num_domains)
  done;

  run (CDL.await cdl) ()

let () = Scheduler.run main
