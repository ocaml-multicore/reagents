module type S = sig
  type t
  type ('a,'b) reagent
  val create  : int -> t
  val get     : t -> (unit, int) reagent
  val inc     : t -> (unit, int) reagent
  val dec     : t -> (unit, int) reagent
  val try_dec : t -> (unit, int option) reagent
end

module Make (Reagents: Reagents.S) : S
  with type ('a,'b) reagent = ('a,'b) Reagents.t
