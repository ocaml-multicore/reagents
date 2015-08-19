module type S = sig
  type t
  type 'a offer
  val empty : t
  val with_CAS : t -> PostCommitCAS.t -> t
  val with_offer : t -> 'a offer -> t
  val try_commit : t -> bool
  val cas_count  : t -> int
  val has_offer  : t -> 'a offer -> bool
  val union : t -> t -> t
  val with_post_commit : t -> (unit -> unit) -> t
  (* val can_cas_immediate : t -> ('a,'b) Reagent.t -> 'b Offer.t -> bool *)
end

module Make (Sched: Scheduler.S) : S with type 'a offer = 'a Offer.Make(Sched).t
