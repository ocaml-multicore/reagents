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
  with type ('a,'b) reagent = ('a,'b) Reagents.t = struct

  type ('a,'b) reagent = ('a,'b) Reagents.t

  open Reagents

  type status = Locked | Unlocked

  type t = status Ref.ref

  let create () = Ref.mk_ref Unlocked

  let acq r = Ref.upd r (fun s () ->
    match s with
    | Unlocked -> Some (Locked, ())
    | Locked -> None)

  let rel r = Ref.upd r (fun s () ->
    match s with
    | Locked -> Some (Unlocked, true)
    | Unlocked -> Some (Unlocked, false))

  let try_acq r = Ref.upd r (fun s () ->
    match s with
    | Unlocked -> Some (Locked, true)
    | Locked -> Some (Locked, false))
end
