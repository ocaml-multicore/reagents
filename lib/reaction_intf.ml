module type S = sig
  type t

  val empty : t
  val with_CAS : t -> PostCommitCas.t -> t
  val with_offer : t -> Offer_id.t -> t
  val try_commit : t -> bool
  val cas_count : t -> int
  val has_offer : t -> Offer_id.t -> bool
  val union : t -> t -> t
  val with_post_commit : t -> (unit -> unit) -> t
end
