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
  type 'a cont
  val suspend : ('a cont -> 'a option) -> 'a
  val resume  : 'a cont -> 'a -> unit
  val fork    : (unit -> unit) -> unit
  val fork_on : (unit -> unit) -> int -> unit
  val yield   : unit -> unit
  val get_tid : unit -> int
  val run     : (unit -> unit) -> unit
end

module Make (S : sig val num_domains : int end) : S = struct

  type queue_num = int
  type 'a cont = ('a, unit) continuation * queue_num

  effect Fork     : (unit -> unit) -> unit
  effect Yield    : unit
  effect Suspend  : ('a cont -> 'a option) -> 'a
  effect Resume   : ('a cont * 'a) -> unit
  effect GetTid   : int

  let fork f      = perform (Fork f)
  let yield ()    = perform Yield
  let suspend f   = perform (Suspend f)
  let resume t v  = perform (Resume (t, v))
  let get_tid ()  = perform GetTid

  effect ForkOn     : (unit -> unit) * int -> unit
  effect NumDomains : int

  let fork_on f dom_id = perform (ForkOn (f, dom_id))
  let num_domains () = perform NumDomains

  let num_threads = Kcas.ref 0

  let sq = Array.init S.num_domains (fun _ -> Lockfree.MSQueue.create ())

  let fresh_tid () = Oo.id (object end)

  let enqueue c dom_id = Lockfree.MSQueue.push (Array.get sq dom_id) c

  let rec dequeue dom_id =
    let b = Kcas.Backoff.create () in
    let queue = Array.get sq dom_id in
    let rec loop () = match Lockfree.MSQueue.pop queue with
      | Some k -> k ()
      | None ->
          if Kcas.get num_threads = 0 then ()
          else ( Kcas.Backoff.once b ; loop () )
    in loop ()
  and spawn f (tid:int) =
    let dom_id = Domain.self () in
    Kcas.incr num_threads;
    begin
      match f () with
      | () -> (Kcas.decr num_threads; dequeue dom_id)
      | effect (Fork f) k ->
          enqueue (fun () -> spawn f (fresh_tid ())) dom_id;
          continue k ()
      | effect Yield k -> enqueue (continue k) dom_id; dequeue dom_id
      | effect (Suspend f) k ->
          ( match f (k, dom_id) with
            | None -> dequeue dom_id
            | Some v -> continue k v )
      | effect (Resume ((t,qid), v)) k ->
          enqueue (fun () -> continue t v) qid;
          continue k ()
      | effect GetTid k -> continue k tid
      | effect NumDomains k -> continue k (S.num_domains)
      | effect (ForkOn (f, dom_id)) k ->
          enqueue (fun () -> spawn f (fresh_tid())) dom_id;
          continue k ()
    end

  let run_with f num_domains =
    let started = Kcas.ref 0 in
    let worker () =
      let b = Kcas.Backoff.create ~max:16 () in
      let rec loop () =
        if Kcas.get started = 1 then dequeue (Domain.self())
        else (Kcas.Backoff.once b; loop ())
      in loop ()
    in
    for i = 1 to num_domains - 1 do
      Domain.spawn worker
    done ;
    spawn (fun () -> Kcas.incr started; f ()) (fresh_tid ())

  let run f = run_with f S.num_domains

end
