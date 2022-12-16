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

module Scheduler = Sched_ws.Make (struct
  let num_domains = 1
  let is_affine = false
end)

module Reagents = Reagents.Make (Scheduler)
module Counter = Reagents.Data.Counter
open Reagents

let main () =
  let c = Counter.create 0 in
  assert (run (Counter.inc c) () == 0); 
  run
    ( Counter.try_dec c >>= fun ov ->
      match ov with
      | Some 1 ->
          Printf.printf "Counter is 0. Further decrement blocks the thread!\n%!";
          constant ()
      | _ -> failwith "impossible" )
    ();
  run (Counter.dec c) () |> ignore;
  ()

let () =
  match Scheduler.run main with
  | exception Sched_ws.All_domains_idle -> ()
  | () -> failwith "should have blocked"
