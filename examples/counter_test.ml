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
module R_data = Reagents_data.Make(Reagents)
module Counter = R_data.Counter
open Scheduler
open Reagents

let id_str () = sprintf "%d:%d" (Domain.self ()) (get_tid ())

let main () =
  printf "[%s] starting main\n" (id_str ());

  (* Test 1 *)
  printf "**** Test 1 ****\n%!";
  let c = Counter.create 0 in
  printf "%d\n%!" @@ run (Counter.get c) ();
  printf "%d\n%!" @@ run (Counter.inc c) ();
  printf "%d\n%!" @@ run (Counter.inc c) ();
  printf "%d\n%!" @@ run (Counter.dec c) ();
  run (Counter.try_dec c >>= (fun ov ->
    match ov with
    | Some 1 -> ( printf "Counter is 0. Further decrement blocks the thread!\n%!";
                  constant () )
    | _ -> failwith "impossible")) ();
  printf "%d\n%!" @@ run (Counter.dec c) ();
  printf "Should not see this\n";

  ()

let () = Scheduler.run main
