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
  val make       : unit -> 'a t
  val equal      : 'a t -> 'b t -> bool
  val is_active  : 'a t -> bool
  val get_id     : 'a t -> int
  val wait       : 'a t -> unit
  val complete   : 'a t -> 'a -> PostCommitCAS.t
  val rescind    : 'a t -> 'a option
  val get_result : 'a t -> 'a option
end

module Make (Sched : Scheduler.S) : S = struct
  open CAS.Sugar

  type 'a status =
    | Empty
    | Waiting of unit Sched.cont
    | Rescinded
    | Completed of 'a

  type 'a t = 'a status CAS.ref

  let make () = ref Empty

  let get_id r = CAS.get_id r

  let equal o1 o2 = get_id o1 = get_id o2

  let is_active o = match !o with
    | Empty | Waiting _ -> true
    | Rescinded | Completed _ -> false

  let wait r = Sched.suspend (fun k ->
    let cas_result =
      CAS.map r (fun v ->
        match v with
        | Empty -> Some (Waiting k)
        | Waiting _ -> failwith "Offer.wait(1)"
        | Completed _ | Rescinded -> None)
    in
    match cas_result with
    (* If CAS was a success, then it is no longer this thread's responsibiliy to
     * resume itself. *)
    | CAS.Success _ -> None
    (* If the CAS failed, then another thread has already changed the offer from
     * [Empty] to [Completed] or [Rescinded]. In this case, thread shouldn't
     * wait. *)
    | CAS.Aborted -> Some ()
    | CAS.Failed  -> failwith "Offer.wait(2)")

  let complete r new_v =
    let old_v = !r in
    match old_v with
    | Waiting k ->
        PostCommitCAS.cas r old_v (Completed new_v) (fun () -> Sched.resume k ())
    | Empty ->
        PostCommitCAS.cas r old_v (Completed new_v) (fun () -> ())
    | Rescinded | Completed _ -> PostCommitCAS.return false (fun () -> ())

  let rescind r =
    let cas_result =
      CAS.map r (fun v ->
        match v with
        | Empty | Waiting _ -> Some Rescinded
        | Rescinded | Completed _ -> None)
    in
    ( begin
        match cas_result with
        | CAS.Success (Waiting t) -> Sched.resume t ()
        | _ -> ()
      end;
      match !r with
      | Rescinded -> None
      | Completed v -> Some v
      | _ -> failwith "Offer.rescind")

  let get_result r =
    match !r with
    | Completed v -> Some v
    | _ -> None
end
