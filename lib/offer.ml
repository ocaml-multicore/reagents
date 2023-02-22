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

module type S = Offer_intf.S

module Make (Sched : Scheduler.S) : S = struct
  module Loc = Kcas.Loc

  type 'a status =
    | Empty
    | Waiting of unit Sched.cont
    | Catalyst
    | Rescinded
    | Completed of 'a

  type 'a t = 'a status Loc.t

  let make () = Loc.make Empty
  let get_id r = Offer_id.make (Loc.get_id r)
  let equal o1 o2 = Loc.get_id o1 = Loc.get_id o2

  let is_active o =
    match Loc.get o with
    | Empty | Waiting _ | Catalyst -> true
    | Rescinded | Completed _ -> false

  let wait r =
    Sched.suspend (fun k ->
        match
          Loc.update r (fun v ->
              match v with
              | Empty -> Waiting k
              | Waiting _ | Catalyst -> failwith "Offer.wait(1)"
              | Completed _ | Rescinded -> raise Exit)
        with
        (* If CAS was a success, then it is no longer this thread's responsibiliy to
         * resume itself. *)
        | _ -> None
        (* If the CAS failed, then another thread has already changed the offer from
         * [Empty] to [Completed] or [Rescinded]. In this case, thread shouldn't
         * wait. *)
        | exception Exit -> Some ())

  let complete r new_v =
    let old_v = Loc.get r in
    match old_v with
    | Waiting k ->
        PostCommitCas.cas r old_v (Completed new_v) (fun () ->
            Sched.resume k ())
    | Catalyst -> PostCommitCas.return true (fun () -> ())
    | Empty -> PostCommitCas.cas r old_v (Completed new_v) (fun () -> ())
    | Rescinded | Completed _ -> PostCommitCas.return false (fun () -> ())

  let rescind r =
    (match
       Loc.update r (fun v ->
           match v with
           | Empty | Waiting _ -> Rescinded
           | Rescinded | Completed _ | Catalyst -> raise Exit)
     with
    | Waiting t -> Sched.resume t ()
    | _ | (exception Exit) -> ());
    match Loc.get r with
    | Rescinded | Catalyst -> None
    | Completed v -> Some v
    | _ -> failwith "Offer.rescind"

  let get_result r = match Loc.get r with Completed v -> Some v | _ -> None

  type catalyst = unit -> unit

  let make_catalyst () =
    let offer = Loc.make Catalyst in
    let cancel () = Loc.set offer Rescinded in
    (offer, cancel)

  let cancel_catalyst f = f ()
end
