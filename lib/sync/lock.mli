module type S = Lock_intf.S

module Make (Base : Base.S) : S with type ('a, 'b) reagent = ('a, 'b) Base.t
