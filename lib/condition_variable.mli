module type S = sig
  type ('a, 'b) reagent
  type lock
  type t

  val create : unit -> t

  val wait : lock -> t -> bool
  (** [wait l c] returns [false] if the lock is not currently held. *)

  val signal : t -> unit
  val broadcast : t -> unit
end

module Make
    (Base : Base.S)
    (Lock : Lock.S with type ('a, 'b) reagent = ('a, 'b) Base.t) :
  S with type ('a, 'b) reagent = ('a, 'b) Base.t and type lock = Lock.t
