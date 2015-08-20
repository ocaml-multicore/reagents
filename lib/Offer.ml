module type S = sig
  type 'a t
  val make       : unit -> 'a t
  val get_id     : 'a t -> int
  val wait       : 'a t -> 'a
  val complete   : 'a t -> 'a -> PostCommitCAS.t
  val rescind    : 'a t -> 'a option
  val get_result : 'a t -> 'a option
end

module Make (Sched : Scheduler.S) : S = struct
  open CAS.Sugar

  type 'a status =
    | Waiting of 'a Sched.cont option
    | Rescinded
    | Completed of 'a

  type 'a t = 'a status CAS.ref

  let make () = ref (Waiting None)

  let get_id r = CAS.get_id r

  let wait r = Sched.suspend (fun k ->
    CAS.map r (fun v ->
      match v with
      | Waiting None -> Some (Waiting (Some k))
      | Waiting (Some _) | Rescinded -> failwith "Offer.wait(1)"
      | Completed _ -> None);
    match !r with
    | Completed answer -> Some answer
    | Waiting (Some _) -> None
    | _ -> failwith "Offer.wait(2)")

  let complete r new_v =
    let old_v = !r in
    match old_v with
    | Waiting (Some k) ->
        PostCommitCAS.cas r old_v (Completed new_v) (fun () -> Sched.resume k new_v)
    | Waiting None ->
        PostCommitCAS.cas r old_v (Completed new_v) (fun () -> ())
    | _ -> PostCommitCAS.return false (fun () -> ())

  let rescind r =
    CAS.map r (fun v ->
      match v with
      | Waiting None -> Some Rescinded
      | Rescinded | Waiting (Some _) -> failwith "Offer.rescind(1)"
      | Completed _ -> None);
    match !r with
    | Rescinded -> None
    | Completed v -> Some v
    | _ -> failwith "Offer.rescind(2)"

  let get_result r =
    match !r with
    | Completed v -> Some v
    | _ -> None
end
