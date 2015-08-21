module type S = sig
  type 'a ref
  type ('a,'b) reagent
  val read : 'a ref -> (unit, 'a) reagent
  val cas  : 'a ref -> 'a -> 'a -> (unit, unit) reagent
  val upd  : 'a ref -> ('a * 'b -> ('a *'c) option) -> ('b,'c) reagent
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
          may_sync = k.may_sync;
          compose = (fun next -> read r (k.compose next));
          try_react }

  let read r = read r Reagent.commit

  let wake_all q =
    let rec loop () =
      match MSQueue.pop q with
      | None -> ()
      | Some (Offer ov) -> ignore (Offer.rescind ov)
    in loop ()

  let can_cas_immediate k rx = function
    | Some _ -> false
    | None -> Reaction.cas_count rx = 0 && k.always_commits

  let rec cas : 'a 'r. 'a ref -> 'a -> 'a -> (unit, 'r) reagent -> (unit,'r) reagent =
    fun r expect update k ->
      let try_react () rx o =
        if can_cas_immediate k rx o then
          if r.data <!= expect --> update then
            ( wake_all r.offers; k.try_react () rx o )
          else Retry
        else
          let cas = PostCommitCAS.cas r.data expect update (fun () -> wake_all r.offers) in
          k.try_react () (Reaction.with_CAS rx cas) o
      in
      { always_commits = false;
        may_sync = k.may_sync;
        compose = (fun next -> cas r expect update (k.compose next));
        try_react }

  let cas r e u = cas r e u Reagent.commit

  let rec upd : 'a 'b 'c 'r. 'a ref -> ('a * 'b -> ('a * 'c) option) -> ('c,'r) reagent -> ('b,'r) reagent =
    fun r f k ->
      let try_react b rx o =
        if can_cas_immediate k rx o then
          let ov = CAS.get r.data in
          match f (ov, b) with
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
          match f (ov, b) with
          | None -> Block
          | Some (nv, c) ->
              let cas = PostCommitCAS.cas r.data ov nv (fun () -> wake_all r.offers) in
              k.try_react c (Reaction.with_CAS rx cas) o
      in
      { always_commits = false;
        may_sync = k.may_sync;
        compose = (fun next -> upd r f (k.compose next));
        try_react }

  let upd r f = upd r f Reagent.commit
end
