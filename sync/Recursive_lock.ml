module type S = sig
  type ('a,'b) reagent
  type t

  val create  : unit -> t
  val acq     : t -> (unit, unit) reagent
  (** [run (rel l) ()] returns [false] if the lock is either not held or held by another thread  *)
  val rel     : t -> (unit, bool) reagent
  (** [run (try_acq l) ()] returns [true] if the lock was successful *)
  val try_acq : t -> (unit, bool) reagent
end

module Make (Reagents: Reagents.S) : S
  with type ('a,'b) reagent = ('a,'b) Reagents.t = struct

  type ('a,'b) reagent = ('a,'b) Reagents.t

  open Reagents

  type thread_id = int
  type count = int

  (** A recursive lock is either recursively locked [count] times by [thread_id] or unlocked *)
  type status = Locked of thread_id * count | Unlocked

  type t = status Ref.ref

  let create () = Ref.mk_ref Unlocked

  let acq r = Ref.upd r (fun s () ->
    let tid = Reagents.get_tid () in
    match s with
    | Unlocked ->
       (* No current owner, take the lock *)
       Some (Locked (tid, 1), ())
    | Locked (owner, count) ->
       begin
         if owner = tid then
           Some (Locked (tid, count + 1), ())
         else
           None
       end)

  let rel r = Ref.upd r (fun s () ->
    let tid = Reagents.get_tid () in
    match s with
    | Unlocked -> Some (Unlocked, false)
    | Locked (owner, count) ->
       begin
         if owner = tid then
           let new_count = count - 1 in
           if new_count = 0 then
             Some (Unlocked, true)
           else
             Some (Locked (tid, new_count), true)
         else
           Some (Locked (owner, count), false)
       end)

  let try_acq r = Ref.upd r (fun s () ->
    let tid = Reagents.get_tid () in
    match s with
    | Unlocked -> Some (Locked (tid, 1), true)
    | Locked (owner, count) ->
       begin
         if owner = tid then
           (* Already the owner, increase lock count *)
           Some (Locked (tid, count + 1), true)
         else
           (* Not the owner, don't wait on lock *)
           Some (Locked (owner, count), false)
       end)
end
