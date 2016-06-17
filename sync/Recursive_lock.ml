module type S = sig
  type ('a,'b) reagent
  type t

  val create  : unit -> t
  val acq     : t -> unit
  val rel     : t -> unit
  (** [try_acq l] returns [true] if the lock was successful *)
  val try_acq : t -> bool
end

let is_none n = match n with None -> true | Some _ -> false

module Make (Reagents: Reagents.S) : S
  with type ('a,'b) reagent = ('a,'b) Reagents.t = struct

  type ('a,'b) reagent = ('a,'b) Reagents.t

  open Reagents

  module CV = Condition_variable.Make(Reagents)
  module Lock = Lock.Make(Reagents)
  module R_data = Reagents_data.Make(Reagents)
  module Count = R_data.Counter
  module GetId = Reagents.GetId

  type owner = int option ref

  (** A recursive lock is a lock, a condition variable, an owner, and a counter *)
  type t = Lock.t * CV.t * owner * Count.t

  let create () = (Lock.create (), CV.create (), ref None, Count.create 0)

  let acq (l, cv, o, c) =
    run (Lock.acq l) () ;
    (match !o with
     | Some co ->
        if co <> (GetId.get_tid ()) then
          begin
            (* Not the owner, wait until there is no owner *)
            while (not (is_none !o)) do ignore (CV.wait l cv) done ;
            o := Some (GetId.get_tid ())
          end
     | None ->
        (* No current owner, take the lock *)
        (o := Some (GetId.get_tid ()))) ;
    ignore (run (Count.inc c) ()) ;
    ignore (run (Lock.rel l) ())

  let rel (l, cv, o, c) =
    run (Lock.acq l) () ;
    ignore (run (Count.dec c) ()) ;
    (* Counter is 0 when rel is called the same amount of times as acq *)
    if (run (Count.get c) () = 0) then
      begin
        (* Release ownership and notify one waiting thread *)
        o := None ;
        CV.signal cv
      end ;
    ignore (run (Lock.rel l) ()) ;
    ()


  let try_acq (l, cv, o, c) =
    run (Lock.acq l) () ;
    let res =
      (match !o with
       | Some co ->
          if co = (GetId.get_tid ())
          then
            (* Already the owner, increase lock count *)
            (ignore (run (Count.inc c) ()) ; true)
          else
            (* Not the owner, don't wait on lock *)
            false
       | None ->
          begin
            (* No current oner, take the lock *)
            o := Some (GetId.get_tid ()) ;
            ignore (run (Count.inc c) ()) ;
            true
          end) in
    ignore (run (Lock.rel l) ()) ;
    res
end
