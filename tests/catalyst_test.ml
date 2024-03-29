module Scheduler = (val Reagents.Toy_scheduler.make 1 ())
module Reagents = Reagents.Make (Scheduler)
module Counter = Reagents.Data.Counter
open Reagents

let message_counter () =
  Scheduler.run (fun () ->
      let receiver_counter = Atomic.make 0 in
      let assert_counter v = assert (Atomic.get receiver_counter == v) in

      let (c1 : (unit, unit) Channel.endpoint), c2 = Channel.mk_chan () in

      let receiver =
        let open Reagents in
        Channel.swap c2 >>> lift (fun () -> Atomic.incr receiver_counter)
      in
      Reagents.Catalyst.catalyse receiver () |> ignore;

      assert_counter 0;
      Reagents.run (Channel.swap c1) ();
      assert_counter 1;
      Reagents.run (Channel.swap c1) ();
      assert_counter 2)

let three_channels_joined () =
  Scheduler.run (fun () ->
      let transferred = Atomic.make 0 in

      let (a1 : (int, unit) Channel.endpoint), a2 = Channel.mk_chan () in
      let (b1 : (int, unit) Channel.endpoint), b2 = Channel.mk_chan () in
      let (c1 : (int, unit) Channel.endpoint), c2 = Channel.mk_chan () in

      let forward receive send =
        let open Reagents in
        Channel.swap receive >>> Channel.swap send
      in
      Reagents.Catalyst.catalyse (forward a2 b1) () |> ignore;
      Reagents.Catalyst.catalyse (forward b2 c1) () |> ignore;

      Scheduler.fork (fun () ->
          let v = Reagents.run (Channel.swap c2) () in
          Atomic.set transferred v);

      Reagents.run (Channel.swap a1) 1;

      while Atomic.get transferred == 0 do
        () (* not necessary with 1 thr *)
      done;
      assert (Atomic.get transferred == 1);
      ())

let message_counter_stress () =
  Scheduler.run (fun () ->
      let receiver_counter = Atomic.make 0 in
      let assert_counter v = assert (Atomic.get receiver_counter == v) in

      let (c1 : (unit, unit) Channel.endpoint), c2 = Channel.mk_chan () in

      let receiver =
        let open Reagents in
        Channel.swap c2 >>> lift (fun () -> Atomic.incr receiver_counter)
      in
      Reagents.Catalyst.catalyse receiver () |> ignore;

      let count = 1_000_000 in
      for _ = 1 to count do
        Reagents.run (Channel.swap c1) (Sys.opaque_identity ())
      done;
      assert_counter count)

let () =
  let open Alcotest in
  run "catalyst test"
    [
      ( "simple",
        [
          test_case "message counter" `Quick message_counter;
          test_case "three channels joined" `Quick three_channels_joined;
          test_case "message counter, 10^6 items" `Quick message_counter_stress;
        ] );
    ]
