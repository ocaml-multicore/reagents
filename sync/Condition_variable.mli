module type S = sig
  type ('a,'b) reagent
  type lock
  type t

  val create : unit -> t
  (** [wait l c] returns [false] if the lock is not currently held. *)
  val wait      : lock -> t -> bool
  val signal    : t -> unit
  val broadcast : t -> unit
end

module Make (Reagents: Reagents.S) : S
  with type ('a,'b) reagent = ('a,'b) Reagents.t
   and type lock = Lock.Make(Reagents).t
