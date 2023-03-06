module type S = sig
  type 'a cont

  val suspend : ('a cont -> 'a option) -> 'a
  val resume : 'a cont -> 'a -> unit
  val fork : (unit -> unit) -> unit
  val yield : unit -> unit
  val get_tid : unit -> int
  val run : (unit -> unit) -> unit

  (* wrapper for tests that are expected to block (be it desirable or not) *)
  val run_allow_deadlock : (unit -> unit) -> unit
end