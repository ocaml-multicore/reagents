module type S = sig
  type ('a,'b) reagent
  type t

  val create  : unit -> t
  val acq     : t -> (unit, unit) reagent
  (** [run (rel l) ()] returns [false] if the lock is not currently held. *)
  val rel     : t -> (unit, bool) reagent
  val try_acq : t -> (unit, bool) reagent
end

module Make (Reagents: Reagents.S) : S
  with type ('a,'b) reagent = ('a,'b) Reagents.t
