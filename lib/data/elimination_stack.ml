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

module type S = Elimination_stack_intf.S

module Make (Base : Base.S) : S with type ('a, 'b) reagent = ('a, 'b) Base.t =
struct
  type ('a, 'b) reagent = ('a, 'b) Base.t

  module TS = Treiber_stack.Make (Base)
  module C = Base.Channel
  open Base

  type 'a t = {
    stack : 'a TS.t;
    elim_push : ('a, unit) C.endpoint;
    elim_pop : (unit, 'a) C.endpoint;
  }

  let create () =
    let elim_push, elim_pop = C.mk_chan () in
    { stack = TS.create (); elim_push; elim_pop }

  let push r = TS.push r.stack <+> C.swap r.elim_push
  let pop r = TS.pop r.stack <+> C.swap r.elim_pop

  let try_pop r =
    let side_chan = C.swap r.elim_pop >>= fun x -> constant (Some x) in
    TS.try_pop r.stack <+> side_chan
end
