module Scheduler = Sched_ws.Make(
  struct
    let num_domains = 4
    let is_affine = false
  end)
module Reagents = Reagents.Make(Scheduler)
open Scheduler
open Reagents

module Sync = Reagents.Sync
module RLock = Sync.Recursive_lock(Scheduler)
module CDL = Sync.Countdown_latch

let flip f a b = f b a

let rec lock_and_call l i =
  flip run () @@ RLock.acq l ;
  callback l i ;
  ignore (flip run () @@ RLock.rel l)
and callback l i = if i > 0 then lock_and_call l (i - 1)

let main () =
  let l = RLock.create () in

  fork_on (fun () ->
      run (RLock.acq l) () ;
      lock_and_call l 1 ;
      ignore (run (RLock.rel l) ())) 2;

  let cdl = CDL.create (100 + (100 * 2)) in
  (* ... *)
  for i = 0 to 99 do
    fork_on (fun () -> lock_and_call l 100 ; run (CDL.count_down cdl) ()) (i mod 4)
  done ;
  (* ... *)
  for i = 0 to 99 do
    fork_on (fun () ->
              run (RLock.acq l) () ;
              if run (RLock.try_acq l) ()
              then ignore @@ run (RLock.rel l) ()
              else assert false ;
              ignore @@ run (RLock.rel l) () ;
              run (CDL.count_down cdl) ())
            (i mod 4);
    fork_on (fun () ->
              if run (RLock.try_acq l) () then
                (lock_and_call l 100 ;
                ignore (run (RLock.rel l) ())) ;
              run (CDL.count_down cdl) ())
            (i mod 4)
  done ;

  run (CDL.await cdl) ()

let () = Scheduler.run main
