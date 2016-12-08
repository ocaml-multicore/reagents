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
  (** The type of continuation. *)

  val suspend : ('a cont -> 'a option) -> 'a
  (** [suspend f] applies [f] to the current continuation. If [f] returns
      [Some v], then the function returns [v] immediately.  Otherwise, if [f]
      returns [None], then the current fiber is suspended and the control
      switches to the next fiber from the scheduler. *)

  val resume  : 'a cont -> 'a -> unit
  (** [resume k v] prepares to resume the continuation [k] with value [v] and
      enqueues the continuation to the scheduler queue. *)

  val get_tid : unit -> int
  (** Return the current thread id. *)
end

module type S = sig

  type ('a,'b) t
  (** The type of a reagent computation which accepts a value of type ['a] and
      returns a value of type ['b]. *)

  val never : ('a,'b) t
  (** A reagent that is never enabled. *)

  val constant : 'a -> ('b,'a) t
  (** [constant v] is a reagent that always returns [v]. *)

  val post_commit : ('a -> unit) -> ('a, 'a) t
  (** [post_commit f] returns a reagent [r] that runs [f r] after the reagent
      [r] (or any reagent constructed using [r]) commits. *)
  
  val lift : ('a -> 'b) -> ('a,'b) t
  (** [lift f] lifts a pure function [f] to a reagent. If [f] includes
      side-effects, then the side-effects may be performed zero or more times.
      It is expected that [f] does not perform any blocking operations. *)

  val lift_blocking : ('a -> 'b option) -> ('a,'b) t
  (** [lift_blocking f] blocks if [f] returns [None]. Otherwise, it behaves
      like {!lift}. *)

  val return : ('a -> (unit, 'b) t) -> ('a,'b) t
  (** The monadic return primitive for reagents. *)

  val (>>=) : ('a,'b) t -> ('b -> (unit,'c) t) -> ('a,'c) t
  (** The monadic bind primitive for reagents. *)

  val (>>>) : ('a,'b) t -> ('b,'c) t -> ('a,'c) t
  (** The sequential composition operator. [a >>> b] perform [a] and [b]
      atomically. Corresponds to arrow bind. *)

  val (<+>) : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
  (** Left-biased choice. [a <+> b] first attempts [a]. If [a] blocks, then [b]
      is attempted. If both of them block, then the whole protocol blocks. *)
 
  val (<*>) : ('a,'b) t -> ('a,'c) t -> ('a, 'b * 'c) t
  (** Parallel composition operator. [a <*> b] is only enabled if both [a] and
      [b] are enabled. *)

  val attempt : ('a,'b) t -> ('a, 'b option) t
  (** Convert a blocking reagent into a non-blocking one. If reagent [r] is a
      blocks, then [attempt r] return [None]. If [r] does not block and returns
      a value [v], then [attempt r] returns [Some v]. *)

  val run : ('a,'b) t -> 'a -> 'b
  (** [run r v] runs the reagents [r] with value [v]. *)

  module Ref : Ref.S with type ('a,'b) reagent = ('a,'b) t
  (** Shared memory references. *)

  module Channel : Channel.S with type ('a,'b) reagent = ('a,'b) t
  (** Synchronous message-passing channels. *)
end

module Make (Sched: Scheduler) : S
