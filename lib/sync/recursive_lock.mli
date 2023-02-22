module type S = Recursive_lock_intf.S

module Make
    (Base : Base.S) (Tid : sig
      val get_tid : unit -> int
    end) : S with type ('a, 'b) reagent = ('a, 'b) Base.t
