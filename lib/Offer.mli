module type S = sig
  type 'a t
  val make       : unit -> 'a t
  val get_id     : 'a t -> int
  val wait       : 'a t -> 'a
  val complete   : 'a t -> 'a -> PostCommitCAS.t
  val rescind    : 'a t -> 'a option
  val get_result : 'a t -> 'a option
end

module Make (Sched : Scheduler.S) : S
