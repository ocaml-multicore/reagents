module type S = sig

  type reaction
  type 'a offer
  type 'a result = Block | Retry | Done of 'a
  type ('a,'b) t =
    { try_react : 'a -> reaction -> 'b offer option -> 'b result;
      compose : 'r. ('b,'r) t -> ('a,'r) t;
      always_commits : bool;
      may_sync : bool }

  val never       : ('a,'b) t
  val constant    : 'a -> ('b,'a) t
  val post_commit : ('a -> unit) -> ('a, 'a) t
  val lift        : ('a -> 'b option) -> ('a,'b) t
  val computed    : ('a -> (unit, 'b) t) -> ('a,'b) t
  val (>>)        : ('a,'b) t -> ('b,'c) t -> ('a,'c) t
  val choose      : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
  val (<+>)       : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
  val attempt     : ('a,'b) t -> ('a, 'b option) t
  val run         : ('a,'b) t -> 'a -> 'b

  (* Private *)
  val commit      : ('a,'a) t
end

module Make (Sched: Scheduler.S) : S
  with type reaction = Reaction.Make(Sched).t
   and type 'a offer = 'a Offer.Make(Sched).t
