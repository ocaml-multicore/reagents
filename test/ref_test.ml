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

open Printf
module Scheduler = Sched_ws.Make(struct let num_domains = 1 end)
module Reagents = Reagents.Make (Scheduler)
open Scheduler
open Reagents
open Reagents.Ref

let id_str () = sprintf "%d:%d" (Domain.self ()) (get_tid ())

let main () =
  printf "[%s] starting main\n" (id_str ());

  (* Test 1 *)
  printf "**** Test 1 ****\n%!";
  let r = mk_ref 0 in
  (* [foo] should be pure. Printing for illustrative purposes. *)
  let foo ov () =
    if ov = 0 then ( printf "[%s] Saw 0. Blocking..\n%!" (id_str ()); None )
    else ( printf "[%s] Saw 1. Success. Set to 2.\n%!" (id_str ()); Some (2, ()) )
  in
  fork (fun () -> run (upd r foo) ());
  run (cas r 0 1) ();
  printf "[%s] CAS 0 --> 1 succeeded\n%!" (id_str ());
  Unix.sleep 1;

  (* Test 2 *)
  printf "**** Test 2 ****\n%!";
  let test2_rg =
    read r >>= fun i ->
    if i <> 3
    then ( printf "[%s] Saw %d. Expecting 3. Blocking..\n%!" (id_str()) i; never )
    else ( printf "[%s] Saw 3. Success.\n%!" (id_str ()); constant () )
  in
  fork (run test2_rg);
  run (cas r 2 3) ();
  printf "[%s] CAS 2 --> 3 succeeded\n%!" (id_str ())

let () = Scheduler.run main
