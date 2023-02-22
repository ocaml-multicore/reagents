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

  let count = 1_000_0000 in
  for _ = 1 to count do
    Reagents.run (Channel.swap c1) (Sys.opaque_identity ())
  done;
  assert_counter count

let () = Scheduler.run main
