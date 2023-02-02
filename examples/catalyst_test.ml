module Scheduler = (val Sched_ws.make 1 ())
module Reagents = Reagents.Make (Scheduler)
module R_data = Reagents.Data
module Counter = R_data.Counter
open Reagents

let main () =
  let receiver_counter = Atomic.make 0 in
  let assert_counter v = assert (Atomic.get receiver_counter == v) in

  let (c1 : (unit, unit) Channel.endpoint), c2 = Channel.mk_chan () in

  let receiver =
    let open Reagents in
    Channel.swap c2 >>> lift (fun () -> Atomic.incr receiver_counter)
  in
  Reagents.catalyse receiver () |> ignore;

  assert_counter 0;
  Reagents.run (Channel.swap c1) ();
  assert_counter 1;
  Reagents.run (Channel.swap c1) ();
  assert_counter 2

let () = Scheduler.run main
