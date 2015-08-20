module type S = sig
  type ('a,'b) t
  val never       : ('a,'b) t
  val constant    : 'a -> ('b,'a) t
  val post_commit : ('a -> unit) -> ('a, 'a) t
  val lift        : ('a -> 'b option) -> ('a,'b) t
  val (>>)        : ('a,'b) t -> ('b,'c) t -> ('a,'c) t
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

  type ('a,'b) mkr_info =
    { ret_val : 'a -> 'b result;
      new_rx  : 'a -> Reaction.t -> Reaction.t }

  let rec mk_reagent : 'a 'b 'r. ('a,'b) mkr_info -> ('b,'r) t -> ('a,'r) t =
    fun m k ->
      { may_sync = k.may_sync;
        always_commits = k.always_commits;
        try_react = (fun a rx o ->
          match m.ret_val a with
          | Done b -> k.try_react b (m.new_rx a rx) o
          | Block -> Block
          | Retry -> Retry);
        compose = (fun next -> mk_reagent m (k.compose next)) }

  let constant (x : 'a) : ('b,'a) t =
    mk_reagent {ret_val = (fun _ -> Done x); new_rx = (fun _ v -> v)} commit

  let retry : ('a,'b) t =
    mk_reagent {ret_val = (fun _ -> Retry); new_rx = (fun _ v -> v)} commit

  let block : ('a,'b) t =
    mk_reagent {ret_val = (fun _ -> Block); new_rx = (fun _ v -> v)} commit

  let post_commit (f : 'a -> unit) : ('a,'a) t =
    let ret_val v = Done v in
    let new_rx v rx = Reaction.with_post_commit rx (fun () -> f v) in
    mk_reagent {ret_val; new_rx} commit

  let lift (f : 'a -> 'b option) : ('a,'b) t =
    let ret_val v =
      match f v with
      | None -> Block
      | Some r -> Done r
    in
    mk_reagent {ret_val; new_rx = (fun _ v -> v)} commit

end
