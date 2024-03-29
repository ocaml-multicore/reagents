module type S = sig
  type t
  type ('a, 'b) reagent

  val create : int -> t
  val get_count : t -> (unit, int) reagent
  val await : t -> (unit, unit) reagent
  val count_down : t -> (unit, unit) reagent
end
