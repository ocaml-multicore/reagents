module type S = sig

  type reaction
  type 'a offer
  type 'a result = Block | Retry | Done of 'a
  type ('a,'b) t =
    { try_react : 'a -> reaction -> 'b offer option -> 'b result;
      compose : 'r. ('b,'r) t -> ('a,'r) t;
      always_commits : bool;
      may_sync : bool }

  val never       : ('a,'b) t
  val constant    : 'a -> ('b,'a) t
  val post_commit : ('a -> unit) -> ('a, 'a) t
  val lift        : ('a -> 'b option) -> ('a,'b) t
  val computed    : ('a -> (unit, 'b) t) -> ('a,'b) t
  val (>>)        : ('a,'b) t -> ('b,'c) t -> ('a,'c) t
  val choose      : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
  val (<+>)       : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
  val attempt     : ('a,'b) t -> ('a, 'b option) t
  val run         : ('a,'b) t -> 'a -> 'b

  (* Private *)
  val commit      : ('a,'a) t
end

module Make (Sched: Scheduler.S) : S
  with type reaction = Reaction.Make(Sched).t
   and type 'a offer = 'a Offer.Make(Sched).t = struct

  module Reaction = Reaction.Make(Sched)
  module Offer = Offer.Make(Sched)

  type reaction = Reaction.t
  type 'a offer = 'a Offer.t

  type 'a result = Block | Retry | Done of 'a

  type ('a,'b) t =
    { try_react : 'a -> Reaction.t -> 'b Offer.t option -> 'b result;
      compose : 'r. ('b,'r) t -> ('a,'r) t;
      always_commits : bool;
      may_sync : bool }

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

  let rec computed : 'a 'b 'r. ('a -> (unit, 'b) t) -> ('b, 'r) t -> ('a, 'r) t =
    fun f k ->
      { may_sync = true;
        always_commits = false;
        compose = (fun next -> computed f (k.compose next));
        try_react = (fun a rx o -> ((f a).compose k).try_react () rx o) }

  let computed f = computed f commit

  let rec choose : 'a 'b 'r. ('a,'b) t -> ('a,'b) t -> ('a,'b) t =
    fun r1 r2 ->
      { always_commits = r1.always_commits && r1.always_commits;
        may_sync = r1.may_sync || r2.may_sync;
        compose = (fun next -> choose (r1.compose next) (r2.compose next));
        try_react = fun a rx offer ->
          match r1.try_react a rx offer with
          | (Done _) as v -> v
          | Block -> r2.try_react a rx offer
          | Retry ->
                match r2.try_react a rx offer with
                | Block | Retry -> Retry
                | v -> v }

  let attempt (r : ('a,'b) t) : ('a,'b option) t =
    choose (r >> lift (fun x -> Some (Some x))) (constant None)

  let (<+>) = choose

  let run r v =
    let b = Backoff.create () in
    let pause () = Backoff.once b in
    let rec without_offer () =
      match r.try_react v Reaction.empty None with
      | Done res -> res
      | Retry ->
        ( pause ();
          if r.may_sync then with_offer () else without_offer () )
      | Block -> with_offer ()
    and with_offer () =
      let offer = Offer.make () in
      match r.try_react v Reaction.empty (Some offer) with
      | Done res ->
          (assert (Offer.get_result offer = Some res); res)
      | f ->
        ( begin
            match f with
            | Block -> Offer.wait offer
            | _ -> pause ()
          end;
          match Offer.rescind offer with
          | Some ans -> ans
          | None -> with_offer () )
    in
    without_offer ()

end