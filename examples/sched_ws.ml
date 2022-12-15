(*
 * Copyright (c) 2015, Théo Laurent <theo.laurent@ens.fr>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

module type S = sig
  type queue_id = int
  type thread_id = int
  type 'a cont

  val suspend : ('a cont -> 'a option) -> 'a
  val resume : 'a cont -> 'a -> unit
  val fork : (unit -> unit) -> unit
  val fork_on : (unit -> unit) -> queue_id -> unit
  val yield : unit -> unit
  val get_qid : unit -> queue_id
  val get_tid : unit -> thread_id
  val run : (unit -> unit) -> unit

  (* wrappers for dealing with broken tests *)
  val run_allow_deadlock : (unit -> unit) -> unit
  val run_with_timeout : (unit -> unit) -> unit
end

module Make (S : sig
  val num_domains : int
  val is_affine : bool
  val work_stealing : bool
end) : S = struct
  open Effect
  open Effect.Deep

  type queue_id = int
  type thread_id = int
  type 'a cont = ('a, unit) continuation * queue_id
  type _ Effect.t += Suspend : ('a cont -> 'a option) -> 'a Effect.t
  type _ Effect.t += Resume : ('a cont * 'a) -> unit Effect.t
  type _ Effect.t += Fork : (unit -> unit) -> unit Effect.t
  type _ Effect.t += ForkOn : (unit -> unit) * queue_id -> unit Effect.t
  type _ Effect.t += Yield : unit Effect.t
  type _ Effect.t += GetTid : thread_id Effect.t

  exception All_domains_idle

  let suspend f = perform (Suspend f)
  let resume t v = perform (Resume (t, v))
  let fork f = perform (Fork f)
  let fork_on f qid = perform (ForkOn (f, qid))
  let yield () = perform Yield
  let get_tid () = perform GetTid
  let num_threads = Atomic.make 0
  let num_idling_domains = Atomic.make 0

  let queues =
    Array.init S.num_domains (fun _ -> Lockfree.Michael_scott_queue.create ())

  let get_queue qid = Array.get queues qid
  let dom_id_to_qid_map = Hashtbl.create S.num_domains
  let first_unmapped_qid = ref 0

  let add_dom_to_qid_map (dom_id : Domain.id) =
    Hashtbl.add dom_id_to_qid_map dom_id !first_unmapped_qid;
    first_unmapped_qid := !first_unmapped_qid + 1

  let get_qid () =
    let b = Lockfree.Backoff.create () in
    let rec loop () =
      try Hashtbl.find dom_id_to_qid_map (Domain.self ())
      with Not_found ->
        Lockfree.Backoff.once b;
        loop ()
    in
    loop ()

  let fresh_tid () = Oo.id (object end)
  let enqueue c qid = Lockfree.Michael_scott_queue.push (get_queue qid) c

  module Deadlock_detection = struct
    let status = Atomic.make 0
    let enable () = Atomic.decr status
    let disable () = Atomic.incr status
    let is_on () = Atomic.get status == 0
  end

  let dequeue qid =
    let b = Lockfree.Backoff.create () in
    let rec loop ~idling =
      let queue =
        let qid =
          if idling && S.work_stealing then Random.int S.num_domains else qid
        in
        get_queue qid
      in
      match Lockfree.Michael_scott_queue.pop queue with
      | Some k ->
          if idling then Atomic.decr num_idling_domains;

          k ()
      | None ->
          if
            S.num_domains == Atomic.get num_idling_domains
            && Deadlock_detection.is_on ()
            && not S.work_stealing
          then raise All_domains_idle;

          if Atomic.get num_threads > 0 then (
            if not idling then Atomic.incr num_idling_domains;

            Lockfree.Backoff.once b;
            loop ~idling:true)
    in
    loop ~idling:false

  let rec spawn f (tid : thread_id) =
    let current_qid = get_qid () in
    Atomic.incr num_threads;
    (* begin *)
    match_with f ()
      {
        retc =
          (fun () ->
            Atomic.decr num_threads;
            dequeue current_qid);
        exnc =
          (fun e ->
            print_string (Printexc.to_string e);
            dequeue current_qid);
        effc =
          (fun (type a) (e : a Effect.t) ->
            match e with
            (* | () -> Atomic.decr num_threads; dequeue current_qid *)
            | Suspend f ->
                Some
                  (fun (k : (a, _) continuation) ->
                    match f (k, current_qid) with
                    | None -> dequeue current_qid
                    | Some v -> continue k v)
            | Resume ((t, qid), v) ->
                Some
                  (fun (k : (a, _) continuation) ->
                    if S.is_affine then (
                      enqueue (fun () -> continue t v) qid;
                      continue k ())
                    else (
                      enqueue (continue k) qid;
                      continue t v))
            | Fork f ->
                Some
                  (fun (k : (a, _) continuation) ->
                    if S.is_affine then (
                      enqueue (fun () -> spawn f (fresh_tid ())) current_qid;
                      continue k ())
                    else (
                      enqueue (continue k) current_qid;
                      spawn f (fresh_tid ())))
            | ForkOn (f, qid) ->
                Some
                  (fun (k : (a, _) continuation) ->
                    Deadlock_detection.disable ();

                    if S.is_affine then (
                      enqueue
                        (fun () ->
                          spawn
                            (fun () ->
                              Deadlock_detection.enable ();
                              f ())
                            (fresh_tid ()))
                        qid;
                      continue k ())
                    else (
                      enqueue
                        (fun () ->
                          Deadlock_detection.enable ();
                          continue k ())
                        qid;
                      spawn f (fresh_tid ())))
            | Yield ->
                Some
                  (fun (k : (a, _) continuation) ->
                    enqueue (continue k) current_qid;
                    dequeue current_qid)
            | GetTid -> Some (fun (k : (a, _) continuation) -> continue k tid)
            | _ -> None (* forward the unhandled effects to the outer handler *));
      }

  let run_with f num_domains =
    let started = Atomic.make 0 in
    let worker () =
      let rec loop () =
        if Atomic.get started = 1 then dequeue (get_qid ()) else loop ()
      in
      loop ()
    in
    add_dom_to_qid_map (Domain.self ());
    for _i = 1 to num_domains - 1 do
      let new_domain = Domain.spawn worker in
      add_dom_to_qid_map (Domain.get_id new_domain)
    done;
    spawn
      (fun () ->
        Atomic.incr started;
        f ())
      (fresh_tid ())

  let run f = run_with f S.num_domains

  let run_allow_deadlock f =
    match run f with exception All_domains_idle -> () | _ -> ()

  let run_with_timeout f =
    let running = Atomic.make true in
    Domain.spawn (fun () ->
        f ();
        Atomic.set running true)
    |> ignore;
    Unix.sleepf 0.5
end
