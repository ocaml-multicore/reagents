module type S = sig
  type ('a,'b) t
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

  module Ref : Ref.S with type ('a,'b) reagent = ('a,'b) t
  module Channel : Channel.S with type ('a,'b) reagent = ('a,'b) t
end

module Make (Sched: Scheduler.S) : S = struct
  include Reagent.Make(Sched)
  module Ref = Ref.Make(Sched)
  module Channel = Channel.Make(Sched)
end
