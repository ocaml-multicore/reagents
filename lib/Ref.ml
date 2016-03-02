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
  : S with type ('a,'b) reagent = ('a,'b) Reagent.Make(Sched).t = struct

  module Offer = Offer.Make (Sched)
  module Reagent = Reagent.Make (Sched)
  module Reaction = Reaction.Make (Sched)

  type mono_offer = Offer : 'a Offer.t -> mono_offer

  open CAS.Sugar

  type 'a ref =
    { data : 'a CAS.ref;
      offers : mono_offer MSQueue.t }

  type ('a,'b) reagent = ('a,'b) Reagent.t

  open Reagent

  let mk_ref v = { data = CAS.ref v; offers = MSQueue.create () }

  let rec read : 'a 'r. 'a ref -> ('a,'r) reagent -> (unit,'r) reagent =
    fun r k ->
      let try_react () rx o =
        let () = match o with
          | None -> ()
          | Some ov -> MSQueue.push r.offers (Offer ov)
        in
        let v = CAS.get r.data in
        k.try_react v rx o
      in
        { always_commits = k.always_commits;
          compose = (fun next -> read r (k.compose next));
          try_react }

  let read r = read r Reagent.commit

  let read_imm r = CAS.get r.data

  let wake_all q =
    let rec loop () =
      match MSQueue.pop q with
      | None -> ()
      | Some (Offer ov) -> ( ignore (Offer.rescind ov); loop () )
    in loop ()

  let rec cas : 'a 'r. 'a ref -> 'a -> 'a -> (unit, 'r) reagent -> (unit,'r) reagent =
    let try_react r expect update k () rx o =
      if can_cas_immediate k rx o then
        if r.data <!= expect --> update then
          ( wake_all r.offers; k.try_react () rx o )
        else Retry
      else
        let c = PostCommitCAS.cas r.data expect update (fun () -> wake_all r.offers) in
        k.try_react () (Reaction.with_CAS rx c) o
    in
    fun r expect update k ->
      { always_commits = false;
        compose = (fun next -> cas r expect update (k.compose next));
        try_react = try_react r expect update k}

  let cas r e u = cas r e u Reagent.commit

  let cas_imm r expect update =
    CAS.(commit @@ cas r.data {expect; update})

  let rec upd : 'a 'b 'c 'r. 'a ref -> ('a -> 'b -> ('a * 'c) option) -> ('c,'r) reagent -> ('b,'r) reagent =
    let try_react r f k b rx o =
      if can_cas_immediate k rx o then
        let ov = CAS.get r.data in
        match f ov b with
        | None -> Block
        | Some (nv, c) ->
            if r.data <!= ov --> nv then
              ( wake_all r.offers; k.try_react c rx o )
            else Retry
      else
        let () = match o with
          | None -> ()
          | Some ov -> MSQueue.push r.offers (Offer ov)
        in
        let ov = CAS.get r.data in
        match f ov b with
        | None -> Block
        | Some (nv, c) ->
            let cas = PostCommitCAS.cas r.data ov nv (fun () -> wake_all r.offers) in
            k.try_react c (Reaction.with_CAS rx cas) o
    in
    fun r f k ->
      { always_commits = false;
        compose = (fun next -> upd r f (k.compose next));
        try_react = try_react r f k}

  let upd r f = upd r f Reagent.commit
end
