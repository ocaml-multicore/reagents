module Make (R : Reagents.S) = struct
  module Counter = Counter.Make(R)
  module Treiber_stack = Treiber_stack.Make(R)
  module Elimination_stack = Elimination_stack.Make(R)
  module MichaelScottQueue = MichaelScottQueue.Make(R)
end
