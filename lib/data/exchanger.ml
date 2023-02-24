module type S = Exchanger_intf.S

module Make (Base : Base.S) :
  Exchanger_intf.S with type ('a, 'b) reagent = ('a, 'b) Base.t = struct
  type ('a, 'b) reagent = ('a, 'b) Base.t

  module C = Base.Channel
  open Base

  type 'a t = ('a, 'a) C.endpoint * ('a, 'a) C.endpoint

  let create = C.mk_chan
  let exchange e = C.swap (fst e) <+> C.swap (snd e)
end
