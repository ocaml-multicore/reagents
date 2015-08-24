module Make (R : Reagents.S) = struct
  module Counter = Counter.Make(R)
  module Treiber_stack = Treiber_stack.Make(R)
  module Elimination_stack = Elimination_stack.Make(R)
  module MichaelScott_queue = MichaelScott_queue.Make(R)
end
