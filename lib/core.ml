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

module type S = Core_intf.S

module Make (Sched : Scheduler.S) :
  S
    with type reaction = Reaction.Make(Sched).t
     and type 'a offer = 'a Offer.Make(Sched).t = struct
  module Reaction = Reaction.Make (Sched)
  module Offer = Offer.Make (Sched)

  type reaction = Reaction.t
  type 'a offer = 'a Offer.t
  type catalyst = Offer.catalyst
  type 'a result = BlockAndRetry | Block | Retry | Done of 'a

  type ('a, 'b) t = {
    try_react : 'a -> Reaction.t -> 'b Offer.t option -> 'b result;
    compose : 'r. ('b, 'r) t -> ('a, 'r) t;
    always_commits : bool;
  }

  let ( >>> ) r1 r2 = r1.compose r2

  let rec never : 'a 'b. ('a, 'b) t =
    {
      try_react = (fun _ _ _ -> Block);
      always_commits = false;
      compose = (fun _ -> never);
    }

  let commit : ('a, 'a) t =
    let try_react a rx = function
      | None ->
          (* No offer *)
          if Reaction.try_commit rx then Done a else Retry
      | Some offer -> (
          match Offer.rescind offer with
          | None ->
              (* Offer rescinded successfully *)
              if Reaction.try_commit rx then Done a else Retry
          | Some a' -> Done a')
    in
    { always_commits = true; compose = (fun next -> next); try_react }

  type ('a, 'b) mkr_info = {
    ret_val : 'a -> 'b result;
    new_rx : 'a -> Reaction.t -> Reaction.t;
  }

  let rec mk_reagent : 'a 'b 'r. ('a, 'b) mkr_info -> ('b, 'r) t -> ('a, 'r) t =
   fun m k ->
    {
      always_commits = k.always_commits;
      try_react =
        (fun a rx o ->
          match m.ret_val a with
          | Done b -> k.try_react b (m.new_rx a rx) o
          | Retry -> Retry
          | Block -> Block
          | BlockAndRetry -> BlockAndRetry);
      compose = (fun next -> mk_reagent m (k.compose next));
    }

  let constant (x : 'a) : ('b, 'a) t =
    mk_reagent { ret_val = (fun _ -> Done x); new_rx = (fun _ v -> v) } commit

  let post_commit (f : 'a -> unit) : ('a, 'a) t =
    let ret_val v = Done v in
    let new_rx v rx = Reaction.with_post_commit rx (fun () -> f v) in
    mk_reagent { ret_val; new_rx } commit

  let lift (f : 'a -> 'b) : ('a, 'b) t =
    let ret_val v = Done (f v) in
    mk_reagent { ret_val; new_rx = (fun _ v -> v) } commit

  (* [f] should be a pure function *)
  let lift_blocking (f : 'a -> 'b option) : ('a, 'b) t =
    let ret_val v = match f v with None -> Block | Some r -> Done r in
    mk_reagent { ret_val; new_rx = (fun _ v -> v) } commit

  let rec return : 'a 'b 'r. ('a -> (unit, 'b) t) -> ('b, 'r) t -> ('a, 'r) t =
   fun f k ->
    {
      always_commits = false;
      compose = (fun next -> return f (k.compose next));
      try_react = (fun a rx o -> ((f a).compose k).try_react () rx o);
    }

  let return f = return f commit
  let ( >>= ) r f = r >>> return f

  let rec ( <+> ) : 'a 'b 'r. ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t =
   fun r1 r2 ->
    {
      always_commits = r1.always_commits && r2.always_commits;
      compose = (fun next -> r1.compose next <+> r2.compose next);
      try_react =
        (fun a rx offer ->
          match r1.try_react a rx offer with
          | Done _ as v -> v
          | Block -> (
              match r2.try_react a rx offer with
              | Retry -> BlockAndRetry
              | v -> v)
          | Retry -> (
              match r2.try_react a rx offer with
              | Block -> BlockAndRetry
              | v -> v)
          | BlockAndRetry -> (
              match r2.try_react a rx offer with
              | Retry | Block -> BlockAndRetry
              | v -> v));
    }

  let attempt (r : ('a, 'b) t) : ('a, 'b option) t =
    r >>> lift (fun x -> Some x) <+> constant None

  let rec first : 'a 'b 'c 'r. ('a, 'b) t -> ('b * 'c, 'r) t -> ('a * 'c, 'r) t
      =
   fun r k ->
    let try_react (a, c) rx offer =
      (r >>> lift (fun b -> (b, c)) >>> k).try_react a rx offer
    in
    {
      always_commits = r.always_commits && k.always_commits;
      compose = (fun next -> first r (k.compose next));
      try_react;
    }

  let first (r : ('a, 'b) t) : ('a * 'c, 'b * 'c) t = first r commit

  let rec second : 'a 'b 'c 'r. ('a, 'b) t -> ('c * 'b, 'r) t -> ('c * 'a, 'r) t
      =
   fun r k ->
    let try_react (c, a) rx offer =
      (r >>> lift (fun b -> (c, b)) >>> k).try_react a rx offer
    in
    {
      always_commits = r.always_commits && k.always_commits;
      compose = (fun next -> second r (k.compose next));
      try_react;
    }

  let second (r : ('a, 'b) t) : ('c * 'a, 'c * 'b) t = second r commit

  let ( <*> ) (r1 : ('a, 'b) t) (r2 : ('a, 'c) t) : ('a, 'b * 'c) t =
    lift (fun a -> (a, a)) >>> first r1 >>> second r2

  let rec with_offer ?offer pause r v =
    let offer =
      match offer with None -> Offer.make () | Some offer -> offer
    in
    match r.try_react v Reaction.empty (Some offer) with
    | Done res -> res
    | f -> (
        (match f with Block -> Offer.wait offer | _ -> pause ());
        match Offer.rescind offer with
        | Some ans -> ans
        | None -> with_offer pause r v)

  let rec without_offer pause r v =
    match r.try_react v Reaction.empty None with
    | Done res -> res
    | Retry ->
        pause ();
        without_offer pause r v
    | BlockAndRetry ->
        pause ();
        with_offer pause r v
    | Block -> with_offer pause r v

  let run r v =
    let b = Lockfree.Backoff.create () in
    let pause () = Lockfree.Backoff.once b in
    without_offer pause r v

  let catalyse r v =
    let offer, catalyst = Offer.make_catalyst () in
    match r.try_react v Reaction.empty (Some offer) with
    | Done _ | Retry -> assert false
    | Block | BlockAndRetry -> catalyst

  let cancel_catalyst = Offer.cancel_catalyst

  let can_cas_immediate k rx = function
    | Some _ -> false
    | None -> Reaction.cas_count rx = 0 && k.always_commits
end
