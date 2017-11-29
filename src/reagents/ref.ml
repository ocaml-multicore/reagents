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
  type ('a,'b) reagent
  val mk_ref   : 'a -> 'a ref
  val read     : 'a ref -> (unit, 'a) reagent
  val read_imm : 'a ref -> 'a
  val cas      : 'a ref -> 'a -> 'a -> (unit, unit) reagent
  val cas_imm  : 'a ref -> 'a -> 'a -> bool
  val upd      : 'a ref -> ('a -> 'b -> ('a *'c) option) -> ('b,'c) reagent
end

module Make(Sched: Scheduler.S)
  : S with type ('a,'b) reagent = ('a,'b) Reagent_core.Make(Sched).t = struct

  module Offer = Offer.Make (Sched)
  module Reagent_core = Reagent_core.Make (Sched)
  module Reaction = Reaction.Make (Sched)

  type mono_offer = Offer : 'a Offer.t -> mono_offer

  type 'a ref =
    { data : 'a Kcas.ref;
      offers : mono_offer Lockfree.MSQueue.t }

  type ('a,'b) reagent = ('a,'b) Reagent_core.t

  open Reagent_core

  let mk_ref v = { data = Kcas.ref v; offers = Lockfree.MSQueue.create () }

  let rec read : 'a 'r. 'a ref -> ('a,'r) reagent -> (unit,'r) reagent =
    fun r k ->
      let try_react () rx o =
        let () = match o with
          | None -> ()
          | Some ov -> Lockfree.MSQueue.push r.offers (Offer ov)
        in
        let v = Kcas.get r.data in
        k.try_react v rx o
      in
        { always_commits = k.always_commits;
          compose = (fun next -> read r (k.compose next));
          try_react }

  let read r = read r Reagent_core.commit

  let read_imm r = Kcas.get r.data

  let wake_all q =
    let rec loop () =
      match Lockfree.MSQueue.pop q with
      | None -> ()
      | Some (Offer ov) -> ( ignore (Offer.rescind ov); loop () )
    in loop ()

  let cas_imm r expect update =
    Kcas.cas r.data expect update

  let rec upd : 'a 'b 'c 'r. 'a ref -> ('a -> 'b -> ('a * 'c) option) -> ('c,'r) reagent -> ('b,'r) reagent =
    let try_react r f k b rx o =
      if can_cas_immediate k rx o then
        let ov = Kcas.get r.data in
        match f ov b with
        | None -> Block
        | Some (nv, c) ->
            if Kcas.cas r.data ov nv then
              ( wake_all r.offers; k.try_react c rx o )
            else Retry
      else
        let () = match o with
          | None -> ()
          | Some ov -> Lockfree.MSQueue.push r.offers (Offer ov)
        in
        let ov = Kcas.get r.data in
        match f ov b with
        | None -> Block
        | Some (nv, c) ->
            let cas = PostCommitCas.cas r.data ov nv (fun () -> wake_all r.offers) in
            k.try_react c (Reaction.with_CAS rx cas) o
    in
    fun r f k ->
      { always_commits = false;
        compose = (fun next -> upd r f (k.compose next));
        try_react = try_react r f k}

  let upd r f = upd r f Reagent_core.commit

  let cas r expect update = upd r (fun current () ->
    if current = expect then Some (update, ()) else None)
end
