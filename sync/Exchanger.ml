(* A two-way exchanger. Unlike channels, exchanger does not distinguish between
 * the two channel endpoints. *)

module type S = sig
  type 'a t
  type ('a,'b) reagent
  val create   : unit -> 'a t
  val exchange : 'a t -> ('a,'a) reagent
end

module Make (Reagents : Reagents.S) : S
  with type ('a,'b) reagent = ('a,'b) Reagents.t = struct

  type ('a,'b) reagent = ('a,'b) Reagents.t
  module C = Reagents.Channel
  open Reagents

  type 'a t = ('a,'a) C.endpoint * ('a,'a) C.endpoint

  let create = C.mk_chan
  let exchange e = C.swap (fst e) <+> C.swap (snd e)
end
