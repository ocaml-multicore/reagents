module Scheduler = Sched_ws.Make (struct
  let num_domains = 1
  let is_affine = false
  let work_stealing = false
end)

module Reagents = Reagents.Make (Scheduler)
module R_data = Reagents.Data
module Counter = R_data.Counter
open Reagents

let main () =
  let transferred = Atomic.make 0 in

  let (a1 : (int, unit) Channel.endpoint), a2 = Channel.mk_chan () in
  let (b1 : (int, unit) Channel.endpoint), b2 = Channel.mk_chan () in
  let (c1 : (int, unit) Channel.endpoint), c2 = Channel.mk_chan () in

  let forward receive send =
    let open Reagents in
    Channel.swap receive >>> Channel.swap send
  in
  Reagents.catalyse (forward a2 b1) () |> ignore;
  Reagents.catalyse (forward b2 c1) () |> ignore;

  Scheduler.fork (fun () ->
      let v = Reagents.run (Channel.swap c2) () in
      Atomic.set transferred v);

  Reagents.run (Channel.swap a1) 1;

  while Atomic.get transferred == 0 do
    () (* not necessary with 1 thr *)
  done;
  assert (Atomic.get transferred == 1);
  ()

let () = Scheduler.run main
