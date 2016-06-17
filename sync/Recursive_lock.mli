module type S = sig
  type ('a,'b) reagent
  type t

  val create  : unit -> t
  val acq     : t -> unit
  val rel     : t -> unit
  (** [try_acq l] returns [true] if the lock was successful *)
  val try_acq : t -> bool
end

module Make (Reagents: Reagents.S) : S
  with type ('a,'b) reagent = ('a,'b) Reagents.t
