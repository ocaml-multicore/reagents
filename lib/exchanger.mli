module type S = sig
  type 'a t
  type ('a, 'b) reagent

  val create : ?name:string -> unit -> 'a t
  val exchange : 'a t -> ('a, 'a) reagent
end

module Make (Base : Base.S) : S with type ('a, 'b) reagent = ('a, 'b) Base.t
