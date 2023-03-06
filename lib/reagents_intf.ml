module type S = sig
  include Base.S

  module Data : sig
    module Counter : Counter.S with type ('a, 'b) reagent = ('a, 'b) t

    module Treiber_stack :
      Treiber_stack.S with type ('a, 'b) reagent = ('a, 'b) t

    module Elimination_stack :
      Elimination_stack.S with type ('a, 'b) reagent = ('a, 'b) t

    module MichaelScott_queue :
      MichaelScott_queue.S with type ('a, 'b) reagent = ('a, 'b) t
  end

  module Sync : sig
    module Countdown_latch :
      Countdown_latch.S with type ('a, 'b) reagent = ('a, 'b) t

    module Exchanger : Exchanger.S with type ('a, 'b) reagent = ('a, 'b) t
    module Lock : Lock.S with type ('a, 'b) reagent = ('a, 'b) t

    module Recursive_lock (Tid : sig
      val get_tid : unit -> int
    end) : Recursive_lock.S with type ('a, 'b) reagent = ('a, 'b) t

    module Condition_variable :
      Condition_variable.S
        with type ('a, 'b) reagent = ('a, 'b) t
         and type lock = Lock.t
  end
end