module Make (R : Reagents.S) = struct
  module Countdown_latch = Countdown_latch.Make(R)
  module Exchanger = Exchanger.Make(R)
  module Lock = Lock.Make(R)
  module Recursive_lock = Recursive_lock.Make(R)
  module Condition_variable = Condition_variable.Make(R)
end
