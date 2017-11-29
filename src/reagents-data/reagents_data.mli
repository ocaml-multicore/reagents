module Make (R : Reagents.S) : sig
  module Counter : Counter.S with type ('a,'b) reagent = ('a,'b) R.t
  module Treiber_stack : Treiber_stack.S with type ('a,'b) reagent = ('a,'b) R.t
  module Elimination_stack : Elimination_stack.S with type ('a,'b) reagent = ('a,'b) R.t
  module MichaelScott_queue : MichaelScott_queue.S with type ('a,'b) reagent = ('a,'b) R.t
end
