module type S = sig
  type ('a, 'b) reagent
  type t

  val create : unit -> t
  val acq : t -> (unit, unit) reagent

  val try_acq : t -> (unit, bool) reagent
  (** [run (try_acq l) ()] returns [true] if the lock was successful *)

  val rel : t -> (unit, bool) reagent
  (** [run (rel l) ()] returns [false] if the lock is either not held or held by another thread  *)
end
