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
  type 'a ref
  (** The type of shared memory reference. *)

  type ('a, 'b) reagent
  (** The type of reagent. *)

  val mk_ref : 'a -> 'a ref
  (** Make a new reference. *)

  val read : 'a ref -> (unit, 'a) reagent
  (** [read r] returns a reagent, which when run returns the value of the reference. *)

  val read_imm : 'a ref -> 'a
  (** [read_imm r] immediately returns the value of the reference. *)

  val cas : 'a ref -> 'a -> 'a -> (unit, unit) reagent
  (** [cas r e u] returns a reagent, which when run attempts to update the
      reference to [u] if the reference has value [e]. Otherwise, the protocol
      is retried until success. The retry is efficient and does not actively
      consume CPU. The fiber is suspended until there is a possibility of
      success. *)

  val cas_imm : 'a ref -> 'a -> 'a -> bool
  (** [cas_imm r e u] attempts to atomically update [r] from [e] to [u]. If
      successful, the function returns [true]. Otherwise, returns [false]. *)

  val upd : 'a ref -> ('a -> 'b -> ('a * 'c) option) -> ('b, 'c) reagent
  (** [upd r f] returns a reagent value which when run applies [f] to the
      current value [c] of the reference and the input value of the reagent. If
      [f] returns, [None] the protocol is retried. If [f] returns [Some v], the
      reference is attempted to atomically update from [c] to [v]. If the
      update, fails the protocol is retried.

      The retry is efficient and does not actively consume CPU. The fiber is
      suspended until there is a possibility of success. *)
end

module Make (Sched : Scheduler.S) :
  S with type ('a, 'b) reagent = ('a, 'b) Core.Make(Sched).t
