(*
 * Copyright (c) 2023, Vesa Karvonen <vesa.a.j.k@gmail.com>
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

module Scheduler = (val Reagents.Toy_scheduler.make 3 ())
module Reagents = Reagents.Make (Scheduler)
open Reagents
module Q = Data.MichaelScott_queue

let write_skew_test () =
  Scheduler.run_allow_deadlock (fun () ->
      let q1 = Q.create () and q2 = Q.create () in

      let push_to_q2 =
        Q.try_pop q1 >>= function
        | None -> constant 42 >>> Q.push q2
        | Some _ -> constant ()
      and push_to_q1 =
        Q.try_pop q2 >>= function
        | None -> constant 24 >>> Q.push q1
        | Some _ -> constant ()
      and clear = Q.try_pop q1 <*> Q.try_pop q2 in

      let barrier = Atomic.make 3 in
      let sync () =
        Atomic.decr barrier;
        while Atomic.get barrier != 0 do
          Domain.cpu_relax ()
        done
      in

      let exit = Atomic.make false in

      Scheduler.fork (fun () ->
          sync ();
          while not (Atomic.get exit) do
            run push_to_q1 ()
          done);
      Scheduler.fork (fun () ->
          sync ();
          while not (Atomic.get exit) do
            run push_to_q2 ()
          done);

      sync ();
      for _ = 1 to 10_000 do
        match run clear () with
        | Some _, Some _ ->
            Atomic.set exit true;
            failwith "write skew!"
        | _ -> ()
      done;
      Atomic.set exit true;

      ())

let () =
  let open Alcotest in
  run "queue"
    [ ("parallel", [ test_case "write-skew test" `Quick write_skew_test ]) ]
