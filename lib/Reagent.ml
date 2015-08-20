module type S = sig
  type ('a,'b) t
  val never    : ('a,'b) t
  val constant : 'a -> ('b,'a) t
  val retry    : ('a,'b) t
  val block    : ('a,'b) t
  val (>>)     : ('a,'b) t -> ('b,'c) t -> ('a,'c) t
end

module Make (Sched: Scheduler.S) : S = struct

  module Reaction = Reaction.Make(Sched)
  module Offer = Offer.Make(Sched)

  type 'a result = Block | Retry | Done of 'a

  type ('a,'b) t =
    { try_react : 'a -> Reaction.t -> 'b Offer.t option -> 'b result;
      compose : 'r. ('b,'r) t -> ('a,'r) t;
      always_commits : bool;
      may_sync : bool }

  type ('a,'b) message =
    Message : 'a * Reaction.t * ('b, 'r) t * 'r Offer.t -> ('a,'b) message

  let (>>) r1 r2 = r1.compose r2

  let rec never : 'a 'b. ('a,'b) t =
    { try_react = (fun _ _ _ -> Block);
      may_sync = false;
      always_commits = false;
      compose = fun _ -> never }

  let commit : ('a,'a) t =
    let try_react a rx = function
      | None -> (* No offer *)
          if Reaction.try_commit rx then Done a else Retry
      | Some offer ->
          match Offer.rescind offer with
          | None -> (* Offer rescinded successfully *)
              if Reaction.try_commit rx then Done a else Retry
          | Some a' -> Done a'
    in
    { always_commits = true;
      may_sync = false;
      compose = (fun next -> next);
      try_react }

  let rec mk_reagent : 'a 'b 'r. 'a result -> ('a,'r) t -> ('b,'r) t =
    fun x k ->
      { may_sync = k.may_sync;
        always_commits = k.always_commits;
        try_react = (fun _ rx o ->
          match x with
          | Done x -> k.try_react x rx o
          | Block -> Block
          | Retry -> Retry);
        compose = (fun next -> mk_reagent x (k.compose next))}

  let constant (x : 'a) : ('b,'a) t = mk_reagent (Done x) commit
  let retry : ('a,'b) t = mk_reagent Retry commit
  let block : ('a,'b) t = mk_reagent Block commit
end
