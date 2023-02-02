(*
 * Copyright (c) 2015, Théo Laurent <theo.laurent@ens.fr>
 * Copyright (c) 2015-2016, KC Sivaramakrishnan <kc@kcsrk.info>
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

let num_items = 10_000
let items_per_dom = num_items / 2
let () = Printf.printf "items_per_domain = %d\n%!" @@ items_per_dom

module M = struct
  let num_domains = 3
  let is_affine = false
end

module S = Sched_ws.Make (M)
module Reagents = Reagents.Make (S)
open Reagents
open Printf

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
          Gc.full_major ();
          let t1 = Unix.gettimeofday () in
          let () = f () in
          let d = Unix.gettimeofday () -. t1 in
          run (d :: acc) (n - 1)
    in
    let r = run [] n in
    get_mean_sd r
end

module type STACK = sig
  type t

  val create : unit -> t * t
  val push : t -> int -> unit
  val pop : t -> t -> int * int
end

module Sync = Reagents.Sync
module CDL = Sync.Countdown_latch

module Test (Stack : STACK) = struct
  let run items_per_domain =
    let q1, q2 = Stack.create () in
    let b = CDL.create 3 in
    (* initialize work *)
    let rec produce id q = function
      | 0 -> ()
      | i ->
          let v = Random.int 1000 in
          Stack.push q v;
          produce id q (i - 1)
    in
    let rec consume = function
      | 0 -> ()
      | i ->
          let _ = Stack.pop q1 q2 in
          consume (i - 1)
    in
    S.fork_on
      (fun () ->
        produce 1 q1 items_per_domain;
        run (CDL.count_down b) ())
      1;
    S.fork_on
      (fun () ->
        produce 2 q2 items_per_domain;
        run (CDL.count_down b) ())
      2;
    consume items_per_domain;
    run (CDL.count_down b) ();
    run (CDL.await b) ()
end

module Data = Reagents.Data
module T = Data.Treiber_stack

module M1 : STACK = struct
  type t = int T.t

  let create () = (T.create (), T.create ())
  let push s v = Reagents.run (T.push s) v

  let pop s1 s2 =
    let a = Reagents.run (T.pop s1) () in
    let b = Reagents.run (T.pop s2) () in
    (a, b)
end

module M2 : STACK = struct
  type t = int T.t

  let create () = (T.create (), T.create ())
  let push s v = Reagents.run (T.push s) v
  let pop s1 s2 = Reagents.run (T.pop s1 <*> T.pop s2) ()
end

module M3 : STACK = struct
  type t = int T.t

  let create () = (T.create (), T.create ())
  let push s v = Reagents.run (T.push s) v

  let pop s1 s2 =
    Reagents.run
      (T.pop s1
      >>> lift (fun _ -> ())
      >>> T.pop s2
      >>> lift (fun _ -> (0, 0))
      <+> (T.pop s2
          >>> lift (fun _ -> ())
          >>> T.pop s1
          >>> lift (fun _ -> (0, 0))))
      ()
end

let main () =
  let module M = Test (M2) in
  let m, sd = Benchmark.benchmark (fun () -> M.run items_per_dom) 5 in
  printf "<*>: mean = %f, sd = %f tp=%f\n%!" m sd (float_of_int num_items /. m);
  Gc.full_major ();

  let module M = Test (M3) in
  let m, sd = Benchmark.benchmark (fun () -> M.run items_per_dom) 5 in
  printf "<+>: mean = %f, sd = %f tp=%f\n%!" m sd (float_of_int num_items /. m);
  Gc.full_major ();

  let module M = Test (M1) in
  let m, sd = Benchmark.benchmark (fun () -> M.run items_per_dom) 5 in
  printf "Non-atomic: mean = %f, sd = %f tp=%f\n%!" m sd
    (float_of_int num_items /. m);
  Gc.full_major ();

  ()

let () = S.run_allow_deadlock main
