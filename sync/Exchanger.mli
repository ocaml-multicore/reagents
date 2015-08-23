module type S = sig
  type 'a t
  type ('a,'b) reagent
  val create   : unit -> 'a t
  val exchange : 'a t -> ('a,'a) reagent
end

module Make (Reagents : Reagents.S) : S
  with type ('a,'b) reagent = ('a,'b) Reagents.t
