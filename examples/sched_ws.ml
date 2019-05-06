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
  val resume  : 'a cont -> 'a -> unit
  val fork    : (unit -> unit) -> unit
  val fork_on : (unit -> unit) -> queue_id -> unit
  val yield   : unit -> unit
  val get_qid : unit -> queue_id
  val get_tid : unit -> thread_id
  val run     : (unit -> unit) -> unit
end

module Make (S : sig
    val num_domains : int
    val is_affine: bool
  end) : S = struct

  type queue_id = int
  type thread_id = int
  type 'a cont = ('a, unit) continuation * queue_id

  effect Suspend    : ('a cont -> 'a option) -> 'a
  effect Resume     : ('a cont * 'a) -> unit
  effect Fork       : (unit -> unit) -> unit
  effect ForkOn     : (unit -> unit) * queue_id -> unit
  effect Yield      : unit
  effect GetTid     : thread_id

  let suspend f     = perform (Suspend f)
  let resume t v    = perform (Resume (t, v))
  let fork f        = perform (Fork f)
  let fork_on f qid = perform (ForkOn (f, qid))
  let yield ()      = perform Yield
  let get_tid ()    = perform GetTid

  let num_threads = Kcas.ref 0

  let queues = Array.init S.num_domains (fun _ -> Lockfree.MSQueue.create ())

  let get_queue qid = Array.get queues qid

  let dom_id_to_qid_map = Hashtbl.create S.num_domains
  let first_unmapped_qid = ref 0

  let add_dom_to_qid_map (dom_id:Domain.id) =
    Hashtbl.add dom_id_to_qid_map dom_id (!first_unmapped_qid);
    first_unmapped_qid := !first_unmapped_qid + 1

  let get_qid () =
    let b = Kcas.Backoff.create () in
    let rec loop () = try Hashtbl.find dom_id_to_qid_map (Domain.self()) with
      | Not_found -> Kcas.Backoff.once b; loop ()
    in loop ()

  let fresh_tid () = Oo.id (object end)

  let enqueue c qid = Lockfree.MSQueue.push (get_queue qid) c

  let dequeue qid =
    let b = Kcas.Backoff.create () in
    let queue = get_queue qid in
    let rec loop () = match Lockfree.MSQueue.pop queue with
      | Some k -> k ()
      | None ->
          if Kcas.get num_threads <> 0 then
            ( Kcas.Backoff.once b ; loop () )
    in loop ()

  let rec spawn f (tid:thread_id) =
    let current_qid = get_qid () in
      Kcas.incr num_threads;
      begin
        match f () with
        | () -> Kcas.decr num_threads; dequeue current_qid
        | effect (Suspend f) k ->
            begin
              match f (k, current_qid) with
                | None -> dequeue current_qid
                | Some v -> continue k v
            end
        | effect (Resume ((t,qid), v)) k ->
            if S.is_affine then
              begin
                enqueue (fun () -> continue t v) qid;
                continue k ()
              end
            else
              begin
                enqueue (continue k) qid;
                continue t v
              end
        | effect (Fork f) k ->
            if S.is_affine then
              begin
                enqueue (fun () -> spawn f (fresh_tid ())) current_qid;
                continue k ()
              end
            else
              begin
                enqueue (continue k) current_qid;
                spawn f (fresh_tid ())
              end
              | effect (ForkOn (f, qid)) k ->
              if S.is_affine then
                begin
                  enqueue (fun () -> spawn f (fresh_tid ())) qid;
                  continue k ()
                end
              else
                begin
                  enqueue (continue k) qid;
                  spawn f (fresh_tid ())
                end
        | effect Yield k -> enqueue (continue k) current_qid; dequeue current_qid
        | effect GetTid k -> continue k tid
      end

  let run_with f num_domains =
    let started = Kcas.ref 0 in
    let worker () =
      let b = Kcas.Backoff.create ~max:16 () in
      let rec loop () =
        if Kcas.get started = 1 then dequeue (get_qid ())
        else (Kcas.Backoff.once b; loop ())
      in loop ()
    in
    add_dom_to_qid_map (Domain.self ());
    for _i = 1 to num_domains - 1 do
      let new_domain = Domain.spawn worker in
        add_dom_to_qid_map (Domain.get_id new_domain)
    done;
    spawn (fun () -> Kcas.incr started; f ()) (fresh_tid ())

  let run f = run_with f S.num_domains

end
