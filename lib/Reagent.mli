(*
 * Copyright (c) 2015, Théo Laurent <theo.laurent@ens.fr>
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

  type reaction
  type 'a offer
  type 'a result = BlockAndRetry | Block | Retry | Done of 'a
  type ('a,'b) t =
    { try_react : 'a -> reaction -> 'b offer option -> 'b result;
      compose : 'r. ('b,'r) t -> ('a,'r) t;
      always_commits : bool }

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

  val commit : ('a,'a) t
  val can_cas_immediate : ('a,'b) t -> reaction -> 'c offer option -> bool
end

module Make (Sched: Scheduler.S) : S
  with type reaction = Reaction.Make(Sched).t
   and type 'a offer = 'a Offer.Make(Sched).t
