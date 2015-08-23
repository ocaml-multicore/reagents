module Make (R : Reagents.S) = struct
  module Counter = Counter.Make(R)
end
