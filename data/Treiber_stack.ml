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

  module Ref = Reagents.Ref

  type ('a,'b) reagent = ('a,'b) Reagents.t

  type 'a t = 'a list Ref.ref

  let create () = Ref.mk_ref []

  let push r = Ref.upd r (fun xs x -> Some (x::xs,()))

  let pop r = Ref.upd r (fun l () ->
    match l with
    | [] -> None
    | x::xs -> Some (xs,x))

  let try_pop r = Ref.upd r (fun l () ->
    match l with
    | [] -> Some ([], None)
    | x::xs -> Some (xs, Some x))
end
