module type S = sig
  type t
  type ('a,'b) reagent
  val create  : int -> t
  val get     : t -> (unit, int) reagent
  val inc     : t -> (unit, int) reagent
  val dec     : t -> (unit, int) reagent
  val try_dec : t -> (unit, int option) reagent
end

module Make (Reagents : Reagents.S) : S
  with type ('a,'b) reagent = ('a,'b) Reagents.t = struct

  module Ref = Reagents.Ref
  type ('a,'b) reagent = ('a,'b) Reagents.t

  type t = int Ref.ref

  let create init = Ref.mk_ref init
  let get r = Ref.read r
  let inc r = Ref.upd r (fun i () -> Some (i+1,i))
  let dec r = Ref.upd r (fun i () -> if i > 0 then Some (i-1,i) else None)
  let try_dec r = Ref.upd r (fun i () ->
    if i > 0 then Some (i-1,Some i) else Some (0, None))
end
