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
let num_items = 1_000_000
let items_per_dom = num_items / num_doms
let () = Printf.printf "items_per_domain = %d\n%!" items_per_dom

module S = (val Reagents.Toy_scheduler.make num_doms ())
module Reagents = Reagents.Make (S)
open Reagents

module type STACK = sig
  type 'a t

  val create : unit -> 'a t
  val push : 'a t -> 'a -> unit
  val pop : 'a t -> 'a option
end

module type RSTACK = sig
  type 'a t

  val create : unit -> 'a t
  val push : 'a t -> ('a, unit) Reagents.t
  val try_pop : 'a t -> (unit, 'a option) Reagents.t
end

module MakeS (RQ : RSTACK) : STACK = struct
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

module Sync = Reagents.Sync
module CDL = Sync.Countdown_latch

module Test (Q : STACK) = struct
  let run num_doms items_per_domain =
    let q : int Q.t = Q.create () in
    let b = CDL.create num_doms in
    (* initialize work *)
    let rec produce = function
      | 0 -> ()
      (* printf "production complete\n%!" *)
      | i ->
          Q.push q i;
          produce (i - 1)
    in
    let rec consume i =
      match Q.pop q with
      | None -> ()
      (* printf "consumed=%d\n%!" i *)
      | Some _ -> consume (i + 1)
    in
    for _ = 1 to num_doms - 1 do
      S.fork (fun () ->
          produce items_per_domain;
          consume 0;
          run (CDL.count_down b) ())
    done;
    produce items_per_domain;
    consume 0;
    run (CDL.count_down b) ();
    run (CDL.await b) ()
end

module Data = Reagents.Data

let main () =
  let module M = Test (MakeS (Data.Elimination_stack)) in
  let m, sd = Benchmark.benchmark (fun () -> M.run num_doms items_per_dom) 10 in
  Printf.printf "Elimination stack: mean = %f, sd = %f tp=%f\n%!" m sd
    (float_of_int num_items /. m)

let () = S.run main
