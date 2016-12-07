module Make (R: Reagents.S) : sig 
  module Countdown_latch : Countdown_latch.S with type ('a,'b) reagent = ('a,'b) R.t
  module Exchanger : Exchanger.S with type ('a,'b) reagent = ('a,'b) R.t
  module Lock : Lock.S with type ('a,'b) reagent = ('a,'b) R.t
  module Recursive_lock : Recursive_lock.S with type ('a,'b) reagent = ('a,'b) R.t
  module Condition_variable : Condition_variable.S 
    with type ('a,'b) reagent = ('a,'b) R.t
     and type lock  = Lock.t
end
