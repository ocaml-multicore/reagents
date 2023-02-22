module type S = sig
  type 'a cont
  (** The type of continuation. *)

  val suspend : ('a cont -> 'a option) -> 'a
  (** [suspend f] applies [f] to the current continuation. If [f] returns
      [Some v], then the function returns [v] immediately.  Otherwise, if [f]
      returns [None], then the current fiber is suspended and the control
      switches to the next fiber from the scheduler. *)

  val resume : 'a cont -> 'a -> unit
  (** [resume k v] prepares to resume the continuation [k] with value [v] and
      enqueues the continuation to the scheduler queue. *)

  val get_tid : unit -> int
  (** Return the current thread id. *)
end
