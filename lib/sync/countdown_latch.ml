module type S = Countdown_latch_intf.S

module Make (Base : Base.S) : S with type ('a, 'b) reagent = ('a, 'b) Base.t =
struct
  type ('a, 'b) reagent = ('a, 'b) Base.t

  module C = Counter.Make (Base)
  open Base

  type t = C.t

  let create = C.create
  let get_count = C.get
  let count_down c = C.try_dec c >>= fun _ -> constant ()

  let await c =
    C.get c >>> lift_blocking (fun v -> if v = 0 then Some () else None)
end
