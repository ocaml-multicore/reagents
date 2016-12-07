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

let mk_tw_chan () =
  let ab,ba = mk_chan ~name:"ab" () in
  let bc,cb = mk_chan ~name:"bc" () in
  let ac,ca = mk_chan ~name:"ac" () in
  (ab,ac), (ba,bc), (ca,cb)

let tw_swap (c1, c2) =
  swap c1 <*> swap c2

let work sw v () =
  let (x,y) = run (tw_swap sw) v in
  Printf.printf "%d %d" x y

let main () =
  let sw1, sw2, sw3 = mk_tw_chan () in
  fork (work sw1 1);
  fork (work sw2 2);
  work sw3 3 ()

let _ = Scheduler.run main
