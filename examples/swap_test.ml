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
open Scheduler
open Reagents
open Reagents.Channel

let test1 () =
  let ep1, ep2 = mk_chan () in
  let fp1, fp2 = mk_chan () in
  fork (fun () -> assert (run (swap fp1 >>> swap ep1) 1 == 2));
  fork (fun () -> assert (run (swap fp2) 0 == 1));
  assert (run (swap ep2) 2 == 0)

let test2 () =
  let ep1, ep2 = mk_chan () in
  fork (fun () -> assert (run (swap ep1 >>> swap ep1) 1 == 0));
  fork (fun () -> assert (run (swap ep2) 0 == 2));
  assert (run (swap ep2) 2 == 1)

let test3 () =
  let ep1, ep2 = mk_chan () in
  fork (fun () -> assert (run (swap ep1 <+> swap ep2) 0 == 1));
  assert (run (swap ep2) 1 == 0)

let test4 () =
  (* Reagents are not as powerful as communicating transactions. *)
  let ep1, ep2 = mk_chan () in
  fork (fun () -> Printf.printf "%d\n%!" (run (swap ep1 >>> swap ep1) 0));
  Printf.printf "%d\n%!" (run (swap ep2 >>> swap ep2) 1)

let _test5 () =
  (* This test should not succeed. *)
  let a, b = mk_chan () in
  let r = Ref.mk_ref 0 in
  fork (fun () -> run (swap a >>> Ref.upd r (fun _ () -> Some (1, ()))) ());
  run (swap b >>> Ref.upd r (fun _ () -> Some (2, ()))) ()

let () =
  Scheduler.run test1;
  Scheduler.run test2;
  Scheduler.run test3;
  match Scheduler.run test4 with exception _ -> () | _ -> assert false
