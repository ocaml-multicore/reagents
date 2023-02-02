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
  type 'a t

  val make : unit -> 'a t
  val equal : 'a t -> 'b t -> bool
  val is_active : 'a t -> bool
  val get_id : 'a t -> Offer_id.t
  val wait : 'a t -> unit
  val complete : 'a t -> 'a -> PostCommitCas.t
  val rescind : 'a t -> 'a option
  val get_result : 'a t -> 'a option

  type catalyst

  val make_catalyst : unit -> 'a t * catalyst
  val cancel_catalyst : catalyst -> unit
end

module Make (Sched : Scheduler.S) : S = struct
  type 'a status =
    | Empty
    | Waiting of unit Sched.cont
    | Catalyst
    | Rescinded
    | Completed of 'a

  type 'a t = 'a status Kcas.ref

  let make () = Kcas.ref Empty
  let get_id r = Offer_id.make (Kcas.get_id r)
  let equal o1 o2 = Kcas.get_id o1 = Kcas.get_id o2

  let is_active o =
    match Kcas.get o with
    | Empty | Waiting _ | Catalyst -> true
    | Rescinded | Completed _ -> false

  let wait r =
    Sched.suspend (fun k ->
        let cas_result =
          Kcas.map r (fun v ->
              match v with
              | Empty -> Some (Waiting k)
              | Waiting _ | Catalyst -> failwith "Offer.wait(1)"
              | Completed _ | Rescinded -> None)
        in
        match cas_result with
        (* If CAS was a success, then it is no longer this thread's responsibiliy to
         * resume itself. *)
        | Kcas.Success _ -> None
        (* If the CAS failed, then another thread has already changed the offer from
         * [Empty] to [Completed] or [Rescinded]. In this case, thread shouldn't
         * wait. *)
        | Kcas.Aborted -> Some ()
        | Kcas.Failed -> failwith "Offer.wait(2)")

  let complete r new_v =
    let old_v = Kcas.get r in
    match old_v with
    | Waiting k ->
        PostCommitCas.cas r old_v (Completed new_v) (fun () ->
            Sched.resume k ())
    | Catalyst -> PostCommitCas.return true (fun () -> ())
    | Empty -> PostCommitCas.cas r old_v (Completed new_v) (fun () -> ())
    | Rescinded | Completed _ -> PostCommitCas.return false (fun () -> ())

  let rescind r =
    let cas_result =
      Kcas.map r (fun v ->
          match v with
          | Empty | Waiting _ -> Some Rescinded
          | Rescinded | Completed _ | Catalyst -> None)
    in
    (match cas_result with
    | Kcas.Success (Waiting t) -> Sched.resume t ()
    | _ -> ());
    match Kcas.get r with
    | Rescinded | Catalyst -> None
    | Completed v -> Some v
    | _ -> failwith "Offer.rescind"

  let get_result r = match Kcas.get r with Completed v -> Some v | _ -> None

  type catalyst = unit -> unit

  let make_catalyst () =
    let offer = Kcas.ref Catalyst in
    let cancel () = Kcas.set offer Rescinded in
    (offer, cancel)

  let cancel_catalyst f = f ()
end
