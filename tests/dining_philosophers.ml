(*
 * Copyright (c) 2015, Theoéo Laurent <theo.laurent@ens.fr>
 * Copyright (c) 2015-2016, KC Sivaramakrishnan <kc@kcsrk.info>
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

let num_philosophers = 3
let num_rounds = 10_000

module Scheduler = (val Sched_ws.make 3 ())
module Reagents = Reagents.Make (Scheduler)
open Reagents
open Channel
module Sync = Reagents.Sync
module CDL = Sync.Countdown_latch

type fork = { drop : (unit, unit) endpoint; take : (unit, unit) endpoint }

let mk_fork () =
  let drop, take = mk_chan () in
  { drop; take }

let drop f = swap f.drop
let take f = swap f.take

let eat l_fork r_fork _i _j =
  ignore @@ run (take l_fork <*> take r_fork) ();
  (* Printf.printf "Philosopher %d eating in round %d\n%!" i j; *)
  Scheduler.fork @@ run (drop l_fork);
  Scheduler.fork @@ run (drop r_fork)

let main () =
  let b = CDL.create num_philosophers in
  let forks = Array.init num_philosophers (fun _ -> mk_fork ()) in
  Array.iter (fun fork -> Scheduler.fork @@ run (drop fork)) forks;

  let work i () =
    let l_fork = forks.(i) in
    let r_fork = forks.((i + 1) mod num_philosophers) in
    for j = 1 to num_rounds do
      eat l_fork r_fork i j
    done;
    run (CDL.count_down b) ()
  in

  for i = 1 to num_philosophers - 1 do
    Scheduler.fork (work i)
  done;
  work 0 ();
  run (CDL.await b) ();
  exit 0

let () = Scheduler.run ~timeout:`Default main
