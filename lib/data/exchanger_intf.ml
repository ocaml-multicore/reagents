module type S = sig
  type 'a t
  type ('a, 'b) reagent

  val create : ?name:string -> unit -> 'a t
  val exchange : 'a t -> ('a, 'a) reagent
end
