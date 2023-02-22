module type S = sig
  type 'a t

  val make : unit -> 'a t
  val equal : 'a t -> 'b t -> bool
  val is_active : 'a t -> bool
  val get_id : 'a t -> Offer_id.t
  val wait : 'a t -> unit
  val complete : 'a t -> 'a -> PostCommitCas.t
  val rescind : 'a t -> 'a option
  val get_result : 'a t -> 'a option

  type catalyst

  val make_catalyst : unit -> 'a t * catalyst
  val cancel_catalyst : catalyst -> unit
end
