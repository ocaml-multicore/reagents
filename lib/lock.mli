module type S = sig
  type ('a, 'b) reagent
  type t

  val create : unit -> t
  val acq : t -> (unit, unit) reagent
  val try_acq : t -> (unit, bool) reagent

  val rel : t -> (unit, bool) reagent
  (** [run (rel l) ()] returns [false] if the lock is not currently held. *)
end

module Make (Base : Base.S) : S with type ('a, 'b) reagent = ('a, 'b) Base.t
