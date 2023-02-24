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

module Scheduler = (val Sched_ws.make 1 ())
module Reagents = Reagents.Make (Scheduler)
open Reagents
module Lock = Sync.Lock
module CV = Sync.Condition_variable

let test () =
  Scheduler.run (fun () ->
      let l = Lock.create () in
      let cv = CV.create () in
      run (Lock.acq l) ();
      Scheduler.fork (fun () ->
          run (Lock.acq l) ();
          CV.signal cv;
          assert (run (Lock.rel l) ()));
      assert (CV.wait l cv);
      assert (run (Lock.rel l) ()))

let () =
  let open Alcotest in
  run "lock test" [ ("simple", [ test_case "lock and cond-var" `Quick test ]) ]
