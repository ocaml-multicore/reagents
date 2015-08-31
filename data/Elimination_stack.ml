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

module type S = sig
  type 'a t
  type ('a,'b) reagent
  val create  : unit -> 'a t
  val push    : 'a t -> ('a, unit) reagent
  val pop     : 'a t -> (unit, 'a) reagent
  val try_pop : 'a t -> (unit, 'a option) reagent
end

module Make (Reagents: Reagents.S) : S
  with type ('a,'b) reagent = ('a,'b) Reagents.t = struct

  type ('a,'b) reagent = ('a,'b) Reagents.t

  module TS = Treiber_stack.Make(Reagents)
  module C = Reagents.Channel
  open Reagents

  type 'a t =
    {stack     : 'a TS.t;
     side_chan : (('a, unit) C.endpoint  (* elim_push *) * (unit,'a) C.endpoint (* elim_pop  *)) array }

  let num_side_chan = 1

  let create () =
    let side_chan = Array.init num_side_chan (fun _ -> C.mk_chan ()) in
    { stack = TS.create (); side_chan }

  let push r =
    let push_chan = fst @@ r.side_chan.(Random.int (num_side_chan)) in
    TS.push r.stack <+> C.swap push_chan

  let pop r =
    let pop_chan = snd @@ r.side_chan.(Random.int (num_side_chan)) in
    TS.pop r.stack <+> C.swap pop_chan

  let try_pop r =
    let pop_chan = snd @@ r.side_chan.(Random.int (num_side_chan)) in
    let peek = C.swap pop_chan >>= (fun x -> constant (Some x)) in
    TS.try_pop r.stack <+> peek

end
