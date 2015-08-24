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

module Sync = Reagents_sync.Make(Reagents)
module Lock = Sync.Lock
module CV = Sync.Condition_variable

let id_str () = sprintf "%d:%d" (Domain.self ()) (get_tid ())

let main () =
  printf "[%s] starting main\n" (id_str ());

  (* Test 1 *)
  printf "**** Test 1 ****\n%!";
  let l = Lock.create () in
  let cv = CV.create () in
  run (Lock.acq l) ();
  printf "[%s] Acquired lock\n" (id_str());
  fork (fun () ->
    printf "[%s] Starting waker thread\n" (id_str ());
    run (Lock.acq l) ();
    printf "[%s] Acquired lock\n" (id_str());
    CV.signal cv;
    printf "[%s] Signal send\n" (id_str());
    assert (run (Lock.rel l) ());
    printf "[%s] Released lock\n" (id_str()));
  printf "[%s] Going to wait on condition variable\n" (id_str());
  assert (CV.wait l cv);
  printf "[%s] Woken up..\n" (id_str());
  assert (run (Lock.rel l) ());
  printf "[%s] Released lock\n" (id_str())

let () = Scheduler.run main
