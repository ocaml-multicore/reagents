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

let print_usage_and_exit () =
  print_endline @@ "Usage: " ^ Sys.argv.(0) ^ " <num_philosophers> <num_rounds>";
  exit(0)

let (num_philosophers, num_rounds) =
  if Array.length Sys.argv < 3 then
    print_usage_and_exit ()
  else
    try
      let a = int_of_string (Sys.argv.(1)) in
      let b = int_of_string (Sys.argv.(2)) in
      (a,b)
    with
    | Failure _ -> print_usage_and_exit ()

module S = Sched_ws_affine.Make (struct
  let num_domains = num_philosophers
end)

module Reagents = Reagents.Make (S)
open Reagents
open Channel

module Sync = Reagents_sync.Make(Reagents)
module CDL  = Sync.Countdown_latch

open Printf

type fork =
  {drop : (unit,unit) endpoint;
   take : (unit,unit) endpoint}

let mk_fork () =
  let drop, take = mk_chan () in
  {drop; take}

let drop f = swap f.drop
let take f = swap f.take

let eat l_fork r_fork i j =
  ignore @@ run (take l_fork <*> take r_fork) ();
  printf "Philosopher %d eating in round %d\n%!" i j;
  S.fork @@ run (drop l_fork);
  S.fork @@ run (drop r_fork)

let main () =
  let b = CDL.create num_philosophers in
  let forks = Array.init num_philosophers (fun _ -> mk_fork ()) in
  Array.iter (fun fork -> S.fork @@ run (drop fork)) forks;

  let work i () =
    let l_fork = forks.(i) in
    let r_fork = forks.((i + 1) mod num_philosophers) in
    for j = 1 to num_rounds do
      eat l_fork r_fork i j
    done;
    printf "[%d] done\n%!" (Domain.self());
    run (CDL.count_down b) ()
  in

  for i = 1 to num_philosophers - 1 do
    S.fork_on i (work i)
  done;
  work 0 ();
  run (CDL.await b) ();
  exit 0

let _ = S.run main
