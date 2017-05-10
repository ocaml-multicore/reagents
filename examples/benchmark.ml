(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

let print_usage_and_exit () =
  print_endline @@ "Usage: " ^ Sys.argv.(0) ^ " <num_domains> <num_items>";
  exit(0)
;;

let (num_doms, num_items) =
  if Array.length Sys.argv < 3 then
    print_usage_and_exit ()
  else try
    let a = int_of_string (Sys.argv.(1)) in
    let b = int_of_string (Sys.argv.(2)) in
    (a,b)
  with Failure _ -> print_usage_and_exit ()
;;

let () =
  if num_doms mod 2 <> 0 then
    (print_endline @@ "<num_domains> must be multiple of 2";
     exit 0)
;;

let items_per_dom = 1 + num_items / num_doms;;

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

  let run num_doms items_per_domain =
    let a = Cas.ref 0 in
    let q : int Q.t = Q.create () in
    let b = CDL.create (num_doms) in
    (* initialize work *)
    let rec produce = function
      |0 -> ()
      |i -> Q.push q i; produce (i-1)
    in
    let rec consume i =
      match Q.pop q with
      |None ->
        let count = run (CDL.get_count b) () in
        Cas.map a (fun c -> Some(c + i)); () (* printf "[%d] consumed=%d    count=%d\n%!" (Domain.self ()) i count *)
      |Some _ -> consume (i+1)
    in
    for i = 0 to num_doms - 1 do
      S.fork_on (fun () ->
        produce items_per_domain;
        consume 0;
        run (CDL.count_down b) ()) i
    done;
    (*produce items_per_domain;*)
    run (CDL.await b) ();
    if Cas.get a < num_items then begin
      print_endline (sprintf "get %d but %d expected" (Cas.get a) num_items);
      assert false
    end
  ;;
end;;

module Data = Reagents_data.Make(Reagents);;

let main () =
  let n = 5 in
(*  let module M = Test(Lock_queue) in
  let (m,sd) = Benchmark.benchmark (fun () -> M.run num_doms items_per_dom) n in
  printf "Lock_queue : mean = %f, sd = %f tp=%f\n%!" m sd (float_of_int num_items /. m);

  let module M = Test(MakeQ(Data.MichaelScott_queue)) in
  let (m,sd) = Benchmark.benchmark (fun () -> M.run num_doms items_per_dom) n in
  printf "Reagent Lockfree.MSQueue: mean = %f, sd = %f tp=%f\n%!" m sd (float_of_int num_items /. m);*)

  let module M = Test(Lockfree.MSQueue) in
  let (m,sd) = Benchmark.benchmark (fun () -> M.run num_doms items_per_dom) n in
  (*printf "Hand-written Lockfree.MSQueue: mean = %f, sd = %f tp=%f\n%!" m sd (float_of_int num_items /. m)*)
  print_endline (sprintf "%f" m)


let () = S.run main
