module type S = sig
  type ('a, 'b) reagent
  type t

  val create : unit -> t
  val acq : t -> (unit, unit) reagent
  val try_acq : t -> (unit, bool) reagent
  val rel : t -> (unit, bool) reagent
end

module Make (Base : Base.S) : S with type ('a, 'b) reagent = ('a, 'b) Base.t =
struct
  type ('a, 'b) reagent = ('a, 'b) Base.t

  open Base

  type status = Locked | Unlocked
  type t = status Ref.ref

  let create () = Ref.mk_ref Unlocked

  let acq r =
    Ref.upd r (fun s () ->
        match s with Unlocked -> Some (Locked, ()) | Locked -> None)

  let rel r =
    Ref.upd r (fun s () ->
        match s with
        | Locked -> Some (Unlocked, true)
        | Unlocked -> Some (Unlocked, false))

  let try_acq r =
    Ref.upd r (fun s () ->
        match s with
        | Unlocked -> Some (Locked, true)
        | Locked -> Some (Locked, false))
end
