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

let num_doms = 4
let num_items = 300_000
let items_per_dom = num_items / num_doms

module S = (val Reagents.Toy_scheduler.make num_doms ())
module Reagents = Reagents.Make (S)

module type QUEUE = sig
  type 'a t

  val create : unit -> 'a t
  val push : 'a t -> 'a -> unit
  val pop : 'a t -> 'a option
end

module type RQUEUE = sig
  type 'a t

  val create : unit -> 'a t
  val push : 'a t -> ('a, unit) Reagents.t
  val try_pop : 'a t -> (unit, 'a option) Reagents.t
end

module MakeQ (RQ : RQUEUE) : QUEUE = struct
  type 'a t = 'a RQ.t

  let create = RQ.create
  let push q v = Reagents.run (RQ.push q) v
  let pop q = Reagents.run (RQ.try_pop q) ()
end

module Benchmark = struct
  let get_mean_sd l =
    let get_mean l =
      List.fold_right (fun a v -> a +. v) l 0. /. (float_of_int @@ List.length l)
    in
    let mean = get_mean l in
    let sd = get_mean @@ List.map (fun v -> abs_float (v -. mean) ** 2.) l in
    (mean, sd)

  let benchmark f n =
    let rec run acc = function
      | 0 -> acc
      | n ->
          let t1 = Unix.gettimeofday () in
          let () = f () in
          let d = Unix.gettimeofday () -. t1 in
          run (d :: acc) (n - 1)
    in
    let r = run [] n in
    get_mean_sd r
end

module CDL = Reagents.Sync.Countdown_latch

module Test (Q : QUEUE) = struct
  let run num_doms items_per_domain =
    let q : int Q.t = Q.create () in
    let b = CDL.create num_doms in
    (* initialize work *)
    let rec produce = function
      | 0 -> ()
      | i ->
          Q.push q i;
          produce (i - 1)
    in
    let rec consume i =
      if i >= items_per_domain then ()
      else match Q.pop q with None -> consume i | Some _ -> consume (i + 1)
    in
    for i = 1 to num_doms - 1 do
      S.fork (fun () ->
          if i mod 2 == 0 then produce items_per_domain else consume 0;
          Reagents.run (CDL.count_down b) ())
    done;
    produce items_per_domain;
    Reagents.run (CDL.count_down b) ();
    Reagents.run (CDL.await b) ()
end

module Make_queue (T : sig
  type 'a t

  val create : unit -> 'a t
  val push : 'a -> 'a t -> unit
  val take_opt : 'a t -> 'a option
end) : QUEUE = struct
  type 'a t = 'a T.t

  let create = T.create
  let push queue value = T.push value queue
  let pop queue = T.take_opt queue
end

let main () =
  let module M = Test (References.Lock_queue) in
  let m, sd = Benchmark.benchmark (fun () -> M.run num_doms items_per_dom) 5 in
  Printf.printf "Lock_queue : mean = %f, sd = %f tp=%f\n%!" m sd
    (float_of_int num_items /. m);

  let module M = Test (References.Two_lock_queue) in
  let m, sd = Benchmark.benchmark (fun () -> M.run num_doms items_per_dom) 5 in
  Printf.printf "Two_lock_queue : mean = %f, sd = %f tp=%f\n%!" m sd
    (float_of_int num_items /. m);

  let module M = Test (Make_queue (Kcas_data.Queue)) in
  let m, sd = Benchmark.benchmark (fun () -> M.run num_doms items_per_dom) 5 in
  Printf.printf "Kcas_data.Queue : mean = %f, sd = %f tp=%f\n%!" m sd
    (float_of_int num_items /. m);

  let module M = Test (MakeQ (Reagents.Data.MichaelScott_queue)) in
  let m, sd = Benchmark.benchmark (fun () -> M.run num_doms items_per_dom) 5 in
  Printf.printf "Reagent Lockfree.MSQueue: mean = %f, sd = %f tp=%f\n%!" m sd
    (float_of_int num_items /. m);

  let module M = Test (Lockfree.Michael_scott_queue) in
  let m, sd = Benchmark.benchmark (fun () -> M.run num_doms items_per_dom) 5 in
  Printf.printf "Hand-written Lockfree.MSQueue: mean = %f, sd = %f tp=%f\n%!" m
    sd
    (float_of_int num_items /. m)

let () = S.run main
