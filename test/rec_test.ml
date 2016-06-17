open Printf
module Scheduler = Sched_ws_affine.Make(struct let num_domains = 4 end)
module Reagents = Reagents.Make(Scheduler)
open Scheduler
open Reagents

module Sync = Reagents_sync.Make(Reagents)
module RLock = Sync.Recursive_lock
module CDL = Sync.Countdown_latch

let rec lock_and_call l i = RLock.acq l ; callback l i ; ignore (RLock.rel l)
and callback l i = if i > 0 then lock_and_call l (i - 1)

let main () =
  let l = RLock.create () in

  fork_on 2 (fun () ->
      run (RLock.acq l) () ;
      lock_and_call l 1 ;
      ignore (run (RLock.rel l) ())) ;

  let cdl = CDL.create (100 + (100 * 2)) in
  (* ... *)
  for i = 0 to 99 do
    fork_on (i mod 4) (fun () -> lock_and_call l 100 ; run (CDL.count_down cdl) ())
  done ;
  (* ... *)
  for i = 0 to 99 do
    fork_on (i mod 4) (fun () -> run (RLock.acq l) () ;
                         if run (RLock.try_acq l) ()
                         then run (RLock.rel l) ()
                         else assert false ;
                         run (RLock.rel l) () ;
                         run (CDL.count_down cdl) ()) ;
    fork_on (i mod 4) (fun () -> if run (RLock.try_acq l) () then
                                   (lock_and_call l 100 ;
                                    ignore (run (RLock.rel l) ())) ;
                                 run (CDL.count_down cdl) ())
  done ;

  run (CDL.await cdl) ()

let () = Scheduler.run main
