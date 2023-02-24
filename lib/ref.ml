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

module type S = Ref_intf.S

module Make (Sched : Scheduler.S) :
  S with type ('a, 'b) reagent = ('a, 'b) Core.Make(Sched).t = struct
  module Loc = Kcas.Loc
  module Op = Kcas.Op
  module Offer = Offer.Make (Sched)
  module Core = Core.Make (Sched)
  module Reaction = Reaction.Make (Sched)

  type mono_offer = Offer : 'a Offer.t -> mono_offer

  type 'a ref = {
    data : 'a Loc.t;
    offers : mono_offer Lockfree.Michael_scott_queue.t;
  }

  type ('a, 'b) reagent = ('a, 'b) Core.t

  open Core

  let mk_ref v =
    { data = Loc.make v; offers = Lockfree.Michael_scott_queue.create () }

  let rec read : 'a 'r. 'a ref -> ('a, 'r) reagent -> (unit, 'r) reagent =
   fun r k ->
    let try_react () reaction offer =
      let () =
        match offer with
        | None -> ()
        | Some offer -> Lockfree.Michael_scott_queue.push r.offers (Offer offer)
      in
      let v = Loc.get r.data in
      k.try_react v reaction offer
    in
    {
      always_commits = k.always_commits;
      compose = (fun next -> read r (k.compose next));
      try_react;
    }

  let read r = read r Core.commit
  let read_imm r = Loc.get r.data

  let wake_all q =
    let rec drain_offers offers =
      match Lockfree.Michael_scott_queue.pop q with
      | None -> offers
      | Some offer -> drain_offers (offer :: offers)
    in
    let offers = drain_offers [] in
    List.iter
      (fun (Offer offer) -> ignore (Option.is_none (Offer.rescind offer)))
      offers

  let cas_imm r expect update = Loc.compare_and_set r.data expect update

  let rec upd :
            'a 'b 'c 'r.
            never_block:bool ->
            'a ref ->
            ('a -> 'b -> ('a * 'c) option) ->
            ('c, 'r) reagent ->
            ('b, 'r) reagent =
   fun ~never_block ref f next_reagent ->
    let on_failure = if never_block then Core.Retry else Core.Block in
    let try_react arg reaction offer =
      if can_cas_immediate next_reagent reaction offer then
        let old_value = Loc.get ref.data in
        match f old_value arg with
        | None -> on_failure
        | Some (new_value, c) ->
            if cas_imm ref old_value new_value then (
              wake_all ref.offers;
              next_reagent.try_react c reaction offer)
            else Retry
      else
        let () =
          match offer with
          | None -> ()
          | Some offer ->
              Lockfree.Michael_scott_queue.push ref.offers (Offer offer)
        in
        let old_value = Loc.get ref.data in
        match f old_value arg with
        | None -> on_failure
        | Some (new_value, return_value) ->
            let cas =
              PostCommitCas.cas ref.data old_value new_value (fun () ->
                  wake_all ref.offers)
            in
            next_reagent.try_react return_value
              (Reaction.with_CAS reaction cas)
              offer
    in
    {
      always_commits = false;
      compose = (fun next -> upd ~never_block ref f (next_reagent.compose next));
      try_react;
    }

  let upd ?(never_block = false) r f = upd ~never_block r f Core.commit

  let cas ?(never_block = false) r expect update =
    upd ~never_block r (fun current () ->
        if current = expect then Some (update, ()) else None)
end
