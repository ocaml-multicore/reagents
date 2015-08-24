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
   and type lock = Lock.Make(Reagents).t = struct

  type ('a,'b) reagent = ('a,'b) Reagents.t

  open Reagents
  module Lock = Lock.Make(Reagents)

  module Q = MichaelScott_queue.Make(Reagents)
  module X = Exchanger.Make(Reagents)

  type lock = Lock.t
  type t = unit X.t Q.t

  let create () = Q.create ()

  let wait l cv =
    let x = X.create () in
    run (constant x >> Q.push cv) ();
    if run (Lock.rel l) () then
      (* Successfully released lock. Wait for signal.. *)
      ( run (X.exchange x) ();
        run (Lock.acq l) ();
        true)
    else
      (* Error! Lock not owned. TODO: Remove/satisfy x.*)
      false

  let signal_bool cv =
    let xo = run (Q.try_pop cv) () in
    match xo with
    | None -> false
    | Some x -> ( run (X.exchange x) (); true )

  let signal cv = ignore (signal_bool cv)

  let rec broadcast cv = if signal_bool cv then broadcast cv else ()
end
