(* A two-way exchanger. Unlike channels, exchanger does not distinguish between
 * the two channel endpoints. *)

module type S = Exchanger_intf.S

module Make (Base : Base.S) : S with type ('a, 'b) reagent = ('a, 'b) Base.t
