module type S = sig
  type reaction
  type 'a offer
  type 'a result = BlockAndRetry | Block | Retry | Done of 'a

  type ('a, 'b) t = {
    try_react : 'a -> reaction -> 'b offer option -> 'b result;
    compose : 'r. ('b, 'r) t -> ('a, 'r) t;
    always_commits : bool;
  }

  val never : ('a, 'b) t
  val constant : 'a -> ('b, 'a) t
  val post_commit : ('a -> unit) -> ('a, 'a) t
  val lift : ('a -> 'b) -> ('a, 'b) t
  val lift_blocking : ('a -> 'b option) -> ('a, 'b) t
  val return : ('a -> (unit, 'b) t) -> ('a, 'b) t
  val ( >>= ) : ('a, 'b) t -> ('b -> (unit, 'c) t) -> ('a, 'c) t
  val ( >>> ) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  val ( <+> ) : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
  val ( <*> ) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t
  val attempt : ('a, 'b) t -> ('a, 'b option) t
  val run : ('a, 'b) t -> 'a -> 'b

  module Catalyst : sig    
    type ('a, 'b) reagent := ('a, 'b) t
    type t

    val catalyse : ('a, 'b) reagent -> 'a -> t
    val cancel : t -> unit
  end

  val commit : ('a, 'a) t
  val can_cas_immediate : ('a, 'b) t -> reaction -> 'c offer option -> bool
end
