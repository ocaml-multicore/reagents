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

module type Scheduler = sig
  type 'a cont
  val suspend : ('a cont -> 'a option) -> 'a
  val resume  : 'a cont -> 'a -> unit
  val get_tid : unit -> int
end

module type S = sig
  type ('a,'b) t
  val never       : ('a,'b) t
  val constant    : 'a -> ('b,'a) t
  val post_commit : ('a -> unit) -> ('a, 'a) t
  val lift        : ('a -> 'b option) -> ('a,'b) t
  val computed    : ('a -> (unit, 'b) t) -> ('a,'b) t
  val (>>=)       : ('a,'b) t -> ('b -> (unit,'c) t) -> ('a,'c) t
  val (>>)        : ('a,'b) t -> ('b,'c) t -> ('a,'c) t
  val choose      : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
  val (<+>)       : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
  val (<*>)       : ('a,'b) t -> ('a,'c) t -> ('a, 'b * 'c) t
  val attempt     : ('a,'b) t -> ('a, 'b option) t
  val run         : ('a,'b) t -> 'a -> 'b
  val get_tid     : unit -> int

  module Ref : Ref.S with type ('a,'b) reagent = ('a,'b) t
  module Channel : Channel.S with type ('a,'b) reagent = ('a,'b) t
end

module Make (Sched: Scheduler) : S = struct
  include Reagent.Make(Sched)
  module Ref = Ref.Make(Sched)
  module Channel = Channel.Make(Sched)

  let get_tid = Sched.get_tid
end
