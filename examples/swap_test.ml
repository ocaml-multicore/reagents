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

open Printf
module Scheduler = Sched_ws.Make(struct let num_domains = 1 end)
module Reagents = Reagents.Make (Scheduler)
open Scheduler
open Reagents
open Reagents.Channel

let id_str () = sprintf "%d:%d" (Domain.self ()) (get_tid ())

let main () =
  printf "[%s] starting main\n" (id_str ());

  (* Test 1 *)
  printf "**** Test 1 ****\n%!";
  let (ep1,ep2) = mk_chan () in
  let (fp1,fp2) = mk_chan () in
  fork (fun () -> printf "[%s] %d\n%!" (id_str ()) @@ run (swap fp1 >> swap ep1) 1);
  fork (fun () -> printf "[%s] %d\n%!" (id_str ()) @@ run (swap fp2) 0);
  printf "[%s] %d\n%!" (id_str ()) @@ run (swap ep2) 2;

  (* Test 2 *)
  yield ();
  Unix.sleep (1);
  printf "**** Test 2 ****\n%!";
  let (ep1,ep2) = mk_chan () in
  fork (fun () -> printf "[%s] %d\n%!" (id_str ()) @@ run (swap ep1 >> swap ep1) 1);
  fork (fun () -> printf "[%s] %d\n%!" (id_str ()) @@ run (swap ep2) 0);
  printf "[%s] %d\n%!" (id_str ()) @@ run (swap ep2) 2;

  (* Test 3 *)
  yield ();
  Unix.sleep (1);
  printf "**** Test 3 ****\n%!";
  let (ep1,ep2) = mk_chan () in
  fork (fun () ->
    printf "[%s] %d\n%!" (id_str ()) @@ run (swap ep1 <+> swap ep2) 0);
  printf "[%s] %d\n%!" (id_str ()) @@ run (swap ep2) 1;

  (* Test 4 *)
  yield ();
  Unix.sleep (1);
  printf "**** Test 4 ****\n%!";
  let (ep1,ep2) = mk_chan () in
  fork (fun () ->
    printf "[%s] %d\n%!" (id_str ()) @@ run (swap ep1 >> swap ep1) 0);
  printf "Will fail! Reagents are not as powerful as communicating transactions!\n%!";
  printf "[%s] %d\n%!" (id_str ()) @@ run (swap ep2 >> swap ep2) 1;
  printf "should not see this!\n";

  ()

let () = Scheduler.run main
