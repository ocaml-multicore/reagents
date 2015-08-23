module type S = sig
  type 'a t
  type ('a,'b) reagent
  val create  : unit -> 'a t
  val push    : 'a t -> ('a, unit) reagent
  val pop     : 'a t -> (unit, 'a) reagent
  val try_pop : 'a t -> (unit, 'a option) reagent
end

module Make (Reagents: Reagents.S) : S
  with type ('a,'b) reagent = ('a,'b) Reagents.t = struct

  type ('a,'b) reagent = ('a,'b) Reagents.t

  module Ref = Reagents.Ref
  open Reagents

  type 'a node =
    | Nil
    | Next of 'a * 'a node Ref.ref

  type 'a t = { head : 'a node Ref.ref; tail : 'a node Ref.ref }

  let create () =
    let init_sentinal = Next (Obj.magic (), Ref.mk_ref Nil) in
    { head = Ref.mk_ref init_sentinal; tail = Ref.mk_ref init_sentinal }

  let pop q = Ref.upd q.head (fun s () ->
    match s with
    | Nil -> failwith "MSQueue.pop: impossible"
    | Next (_,x) ->
        ( match run (Ref.read x) () with
          | Nil -> None
          | Next (v,_) as n -> Some (n,v)))

  let try_pop q = Ref.upd q.head (fun s () ->
    match s with
    | Nil -> failwith "MSQueue.try_pop: impossible"
    | Next (_,x) as n ->
        ( match run (Ref.read x) () with
          | Nil -> Some (n, None)
          | Next (v,_) as n -> Some (n, Some v)))

  let rec find_and_enq n tail =
    match run (Ref.read tail) () with
    | Nil -> failwith "MSQueue.push: impossible"
    | Next (_,r) as ov ->
        let s = run (Ref.read r) () in
        let fwd_tail nv () = ignore @@ run (attempt @@ Ref.cas tail ov nv) () in
        match s with
        | Nil -> Ref.cas r s n >> post_commit (fwd_tail n)
        | Next (_,_) as nv -> ( fwd_tail nv; find_and_enq n tail )

  let push q = computed (fun x ->
    let new_node = Next (x, Ref.mk_ref Nil) in
    find_and_enq new_node q.tail)
end
