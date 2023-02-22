module type S = sig
  type 'a t
  type ('a, 'b) reagent

  val create : unit -> 'a t
  val push : 'a t -> ('a, unit) reagent
  val pop : 'a t -> (unit, 'a) reagent
  val try_pop : 'a t -> (unit, 'a option) reagent
end
