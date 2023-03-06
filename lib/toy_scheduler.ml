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

exception All_domains_idle

module Make (S : sig
  val num_domains : int

  val raise_if_all_idle : bool
  (** [raise_if_all_idle] may throw spuriously with multiple domains. *)
end) : Toy_scheduler_intf.S = struct
  open Effect
  open Effect.Deep

  type 'a cont = ('a, unit) continuation
  type _ Effect.t += Suspend : ('a cont -> 'a option) -> 'a Effect.t
  type _ Effect.t += Resume : ('a cont * 'a) -> unit Effect.t
  type _ Effect.t += Fork : (unit -> unit) -> unit Effect.t
  type _ Effect.t += Yield : unit Effect.t
  type _ Effect.t += GetTid : int Effect.t

  exception All_domains_idle

  let suspend f = perform (Suspend f)
  let resume t v = perform (Resume (t, v))
  let fork f = perform (Fork f)
  let yield () = perform Yield
  let get_tid () = perform GetTid

  type t = {
    num_threads : int Atomic.t;
    num_idling_domains : int Atomic.t;
    mutable domain_ids : Domain.id list;
    queues :
      (Domain.id, (unit -> unit) Lockfree.Michael_scott_queue.t) Hashtbl.t;
    current_tid : int Atomic.t;
  }

  let fresh_tid t = Atomic.fetch_and_add t.current_tid 1

  let enqueue t (task : unit -> unit) =
    let queue = Hashtbl.find t.queues (Domain.self ()) in
    Lockfree.Michael_scott_queue.push queue task

  let take_one t (source : [ `Own | `Any ]) =
    let domain_id =
      match source with
      | `Own -> Domain.self ()
      | `Any ->
          let k = Random.int (List.length t.domain_ids) in
          List.nth t.domain_ids k
    in
    let queue = Hashtbl.find t.queues domain_id in
    Lockfree.Michael_scott_queue.pop queue

  let dequeue t =
    let rec loop ~idling =
      match take_one t (if idling then `Any else `Own) with
      | Some k ->
          if idling then Atomic.decr t.num_idling_domains;
          k ()
      | None ->
          if
            S.raise_if_all_idle
            && S.num_domains == Atomic.get t.num_idling_domains
          then raise All_domains_idle;

          if Atomic.get t.num_threads == 0 then ()
          else (
            if not idling then Atomic.incr t.num_idling_domains;
            loop ~idling:true)
    in
    loop ~idling:false

  let rec spawn (t : t) f =
    let tid = fresh_tid t in
    Atomic.incr t.num_threads;
    (* begin *)
    match_with f ()
      {
        retc =
          (fun () ->
            Atomic.decr t.num_threads;
            dequeue t);
        exnc =
          (fun e ->
            Printf.eprintf "uncaught exn: %s%!" (Printexc.to_string e);
            Stdlib.exit 1);
        effc =
          (fun (type a) (e : a Effect.t) ->
            match e with
            | Suspend f ->
                Some
                  (fun (k : (a, _) continuation) ->
                    match f k with None -> dequeue t | Some v -> continue k v)
            | Resume (l, v) ->
                Some
                  (fun (k : (a, _) continuation) ->
                    enqueue t (continue k);
                    continue l v)
            | Fork f ->
                Some
                  (fun (k : (unit, unit) continuation) ->
                    enqueue t (continue k);
                    spawn t f)
            | Yield ->
                Some
                  (fun (k : (a, _) continuation) ->
                    enqueue t (continue k);
                    dequeue t)
            | GetTid -> Some (fun (k : (a, _) continuation) -> continue k tid)
            | _ -> None (* forward the unhandled effects to the outer handler *));
      }

  let run f =
    let t =
      {
        num_threads = Atomic.make 0;
        num_idling_domains = Atomic.make 0;
        domain_ids = [];
        queues = Hashtbl.create 32;
        current_tid = Atomic.make 0;
      }
    in

    let started = Atomic.make false in
    let worker () =
      let rec loop () = if Atomic.get started then dequeue t else loop () in
      loop ()
    in
    let domains =
      let spawned =
        List.init (S.num_domains - 1) (fun _ -> Domain.spawn worker)
        |> List.map Domain.get_id
      in
      Domain.self () :: spawned
    in
    List.iter
      (fun domain_id ->
        Hashtbl.add t.queues domain_id (Lockfree.Michael_scott_queue.create ()))
      domains;
    t.domain_ids <- domains;

    spawn t (fun () ->
        Atomic.set started true;
        f ())

  let run_allow_deadlock f =
    match run f with exception All_domains_idle -> () | _ -> ()
end

let make ?(raise_if_all_idle = false) num_domains () =
  let module M = Make (struct
    let num_domains = num_domains
    let raise_if_all_idle = raise_if_all_idle
  end) in
  (module M : Toy_scheduler_intf.S)
