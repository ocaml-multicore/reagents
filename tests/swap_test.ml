(*
 * Copyright (c) 2015, Théo Laurent <theo.laurent@ens.fr>
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
open Reagents.Channel

let two_chan_passthrough () =
  Scheduler.run ~timeout:`Default (fun () ->
      let ep1, ep2 = mk_chan () in
      let fp1, fp2 = mk_chan () in
      Scheduler.fork (fun () -> assert (run (swap fp1 >>> swap ep1) 1 == 2));
      Scheduler.fork (fun () -> assert (run (swap fp2) 0 == 1));
      assert (run (swap ep2) 2 == 0))

let one_chan_loopback () =
  Scheduler.run ~timeout:`Default (fun () ->
      let ep1, ep2 = mk_chan () in
      Scheduler.fork (fun () -> assert (run (swap ep1 >>> swap ep1) 1 == 0));
      Scheduler.fork (fun () -> assert (run (swap ep2) 0 == 2));
      assert (run (swap ep2) 2 == 1))

let chan_with_choice () =
  Scheduler.run ~timeout:`Default (fun () ->
      let ep1, ep2 = mk_chan () in
      Scheduler.fork (fun () -> assert (run (swap ep1 <+> swap ep2) 0 == 1));
      assert (run (swap ep2) 1 == 0))

let chan_swap_on_overlapping_locs () =
  (* Reagents are not as powerful as communicating transactions. *)
  Scheduler.run_allow_deadlock ~timeout:`Default (fun () ->
      let ep1, ep2 = mk_chan () in
      Scheduler.fork (fun () ->
          Printf.printf "%d\n%!" (run (swap ep1 >>> swap ep1) 0));
      Printf.printf "%d\n%!" (run (swap ep2 >>> swap ep2) 1))

let ref_upd_on_overlapping_locs () =
  (* This test should not succeed; expecting kcas failure *)
  Scheduler.run_allow_deadlock ~timeout:`Default (fun () ->
      let a, b = mk_chan () in
      let r = Ref.mk_ref 0 in
      Scheduler.fork (fun () ->
          run (swap a >>> Ref.upd r (fun _ () -> Some (1, ()))) ());
      match run (swap b >>> Ref.upd r (fun _ () -> Some (2, ()))) () with
      | exception _ -> ()
      | _ -> assert false)

let () =
  let open Alcotest in
  run "channel test"
    [
      ( "simple",
        [
          test_case "two channels connected" `Quick two_chan_passthrough;
          test_case "one channel, pass item back" `Quick one_chan_loopback;
          test_case "channel with choice" `Quick chan_with_choice;
          test_case "overlapping locations; blocking" `Quick
            chan_swap_on_overlapping_locs;
          test_case "overlapping locations; failing" `Quick
            ref_upd_on_overlapping_locs;
        ] );
    ]
