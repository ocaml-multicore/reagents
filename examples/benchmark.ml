(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

let print_usage_and_exit () =
  print_endline @@ "Usage: " ^ Sys.argv.(0) ^ " <num_domains> <num_task> <num_items>";
  exit(0)
;;

let (num_doms, num_task, num_items) =
  if Array.length Sys.argv < 4 then
    print_usage_and_exit ()
  else try
    let a = int_of_string (Sys.argv.(1)) in
    let b = int_of_string (Sys.argv.(2)) in
    let c = int_of_string (Sys.argv.(3)) in
    (a,b,c)
  with Failure _ -> print_usage_and_exit ()
;;

let () =
  if num_doms mod 2 <> 0 then
    (print_endline @@ "<num_domains> must be multiple of 2";
     exit 0)
;;

let item_per_task = 1 + num_items / num_task;;

(*let () = Printf.printf "[%d] items_per_domain = %d\n%!" (Domain.self ()) items_per_dom;;*)

module S = Sched_ws.Make(struct
  let num_domains = num_doms;;
end);;

module type BARRIER = sig
  type t
  val create : int -> t
  val finish : t -> unit
end;;

module Reagents = Reagents.Make(S);;
open Reagents;;

open Printf;;

module type QUEUE = sig
  type 'a t;;
  val create : unit -> 'a t;;
  val push   : 'a t -> 'a -> unit;;
  val pop    : 'a t -> 'a option;;
end;;

module type RQUEUE = sig
  type 'a t;;
  val create  : unit -> 'a t;;
  val push    : 'a t -> ('a, unit) Reagents.t;;
  val try_pop : 'a t -> (unit, 'a option) Reagents.t;;
end;;

module MakeQ (RQ : RQUEUE) : QUEUE = struct
  type 'a t = 'a RQ.t;;
  let create = RQ.create;;
  let push q v = Reagents.run (RQ.push q) v;;
  let pop q = Reagents.run (RQ.try_pop q) ();;
end;;

module Benchmark = struct
  let get_mean_sd l =
    let get_mean l = (List.fold_right (fun a v -> a +. v) l 0.) /. (float_of_int @@ List.length l) in
    let mean = get_mean l in
    let sd = get_mean @@ List.map (fun v -> abs_float (v -. mean) ** 2.) l in
    (mean, sd)
  ;;

  let benchmark f n =
    let rec run acc = function
    |0 -> acc
    |n ->
      let t1 = Unix.gettimeofday () in
      let () = f () in
      let d = Unix.gettimeofday () -. t1 in
      run (d::acc) (n-1)
    in
    let r = run [] n in
    get_mean_sd r
  ;;
end;;

module Sync = Reagents_sync.Make(Reagents);;
module CDL  = Sync.Countdown_latch;;

module Test (Q : QUEUE) = struct
  module Cas = Kcas.W1;;

(*  let q : int Q.t = Q.create ();;
  let () =
    let rec produce nb =
      match nb with
      |0 -> ()
      |i -> Q.push q i; produce (i-1)
    in produce num_items;;*)

  let run () =
    let a = Cas.ref 0 in
(*    let len = Cas.ref 0 in*)
    let q : int Q.t = Q.create () in
    let b = CDL.create (num_task) in
    (* initialize work *)
    let rec produce nb =
      match nb with
      |0 -> ()
      |i -> Q.push q i;
(*      Cas.incr len;*)
      produce (i-1)
    in
    let rec consume nb =
      match Q.pop q with
      |Some(_) when nb > 0 ->
      Cas.incr a;
(*      Cas.decr len;*)
      consume (nb-1)
      |_ -> ()
      (*print_endline (sprintf "TH%d : still %d to be pop (%d per task) (queue length %d)" (Domain.self ()) nb item_per_task (Cas.get len));*)
    in
    for i = 0 to num_task - 1 do
      S.fork (fun () ->
                produce (2*item_per_task);
                consume item_per_task;
                run (CDL.count_down b) ())
    done;
(*    produce item_per_task;*)
    run (CDL.await b) ();
(*    print_endline (sprintf "Elements consumed and produced : %d/%d" (Cas.get a) (item_per_task * num_task));*)
    if Cas.get a < num_items then begin
      print_endline (sprintf "get %d but %d expected" (Cas.get a) num_items);
      assert false
    end (*else
      print_endline (sprintf "SUCCESS item consume %d (thread: %d, %d)" (Cas.get a) num_doms num_items);*)
  ;;
end;;

module Data = Reagents_data.Make(Reagents);;

let main () =
  let n = 10 in

  let module M = Test(Lockfree.List) in
  let (m,sd) = Benchmark.benchmark (fun () -> M.run ()) n in
  (*printf "Hand-written Lockfree.MSQueue: mean = %f, sd = %f tp=%f\n%!" m sd (float_of_int num_items /. m)*)
  print_endline (sprintf "%f" m)


let () = S.run main
