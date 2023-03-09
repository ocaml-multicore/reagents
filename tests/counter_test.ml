(*
 * Copyright (c) 2015, KC Sivaramakrishnan <sk826@cl.cam.ac.uk>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

module Scheduler = (val Sched_ws.make ~raise_if_all_idle:true 1 ())
module Reagents = Reagents.Make (Scheduler)
open Reagents
module Counter = Data.Counter

let counter () =
  Scheduler.run ~timeout:`Default (fun () ->
      let c = Counter.create 0 in
      assert (run (Counter.get c) () == 0);
      assert (run (Counter.inc c) () == 0);
      assert (run (Counter.inc c) () == 1);
      assert (run (Counter.dec c) () == 2);
      assert (run (Counter.get c) () == 1))

let counter_await_value () =
  Scheduler.run_allow_deadlock ~timeout:`Default (fun () ->
      let c = Counter.create 0 in
      assert (run (Counter.inc c) () == 0);
      run
        ( Counter.try_dec c >>= fun ov ->
          match ov with
          | Some 1 ->
              Printf.printf
                "Counter is 0. Further decrement blocks the thread!\n%!";
              constant ()
          | _ -> failwith "impossible" )
        ();
      run (Counter.dec c) () |> ignore;
      ())

let () =
  let open Alcotest in
  run "counter test"
    [
      ( "simple",
        [
          test_case "get, inc, dec" `Quick counter;
          test_case "blocking" `Quick counter_await_value;
        ] );
    ]
