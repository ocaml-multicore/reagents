(*
########
Copyright (c) 2017, Nicolas ASSOUAD <nicolas.assouad@ens.fr>
########
*)

let print_usage_and_exit () =
  print_endline @@ "Usage: " ^ Sys.argv.(0) ^ " <num_domains> <num_cas> <num_iter>";
  exit(0)
;;

let (num_doms, num_cas, num_iter) =
  if Array.length Sys.argv < 4 then
    print_usage_and_exit ()
  else try
    let a = int_of_string (Sys.argv.(1)) in
    let b = int_of_string (Sys.argv.(2)) in
    let c = int_of_string (Sys.argv.(3)) in
    (a,b,c)
  with Failure _ -> print_usage_and_exit ()
;;

let print s =
  print_endline s
;;

module S = Sched_work_stealing.Make(struct
  let num_domains = num_doms;;
end);;

module Reagents = Reagents.Make(S);;
open Reagents;;

open Printf;;

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

let loop_wait = 100000;;

module Sync = Reagents_sync.Make(Reagents);;
module CDL  = Sync.Countdown_latch;;

module Test = struct
  module Cas = Kcas.W1;;

  let my_swap c str = Channel.swap c;;
(*    (Channel.swap c) >>> (lift (fun a -> print (sprintf str (Domain.self ()))));;*)

  let rec ghost_swap n cout =
(*    print (sprintf "TH%d GHOST_READ ITER n%d" (Domain.self ()) n);*)
    if n > 0 then begin
      run (my_swap cout "TH%d SWAPOUT DONE") ();
      ghost_swap (n-1) cout
    end else ()
(*      print (sprintf "TH%d GHOST_READ END" (Domain.self ()))*)
  ;;

  let new_chan n =
(*    print (sprintf "TH%d NEW_CHAN n°%d" (Domain.self ()) n);*)
    let (cin, cout) = Channel.mk_chan () in
    S.fork (fun () ->
(*      print (sprintf "TH%d GHOST_READ %d" (Domain.self ()) n);*)
      ghost_swap num_iter cout);
    cin
  ;;

  let init_chan n =
    let rec loop n out =
(*      print (sprintf "TH%d INIT_CHAN %d" (Domain.self ()) n);*)
      if n > 0 then
        loop (n-1) ((new_chan n)::out)
      else
        out
    in loop n []
  ;;

  let swap_n n =
    let rec make_reagent l reagent =
      match l with
      |cin::t -> make_reagent t (((my_swap cin "TH%d SWAPIN DONE") <*> reagent) >>> (constant ()))
      |[] -> reagent
    in make_reagent (init_chan n) (constant ())
  ;;

  let run () =
    let a = Cas.ref 0 in
    let b = CDL.create 1 in
    let main_reagent = swap_n num_cas in
    print (sprintf "TH%d BEGINNING TEST" (Domain.self ()));
    for i = 1 to num_iter do
      S.fork (fun () ->
      run main_reagent ();
      run (CDL.count_down b) (); ())
    done;
    run (CDL.await b) ();
    print (sprintf "TH%d END TEST" (Domain.self ()));
    ()
  ;;
end;;

let main () =
  let n = 5 in
  let (m,sd) = Benchmark.benchmark (fun () -> Test.run ()) n in
  print_endline (sprintf "%f" m)
;;

let () = S.run main
