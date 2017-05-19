#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "reagents" @@ fun c ->
  let example x = Pkg.test ~run:false ("examples/" ^ x) in
  Ok [ Pkg.mllib "src/reagents.mllib";
       example "ref_test"; example "swap_test"; example "counter_test"; (* example "hash_test" ;*)
       example "queue_test"; example "stack_test"; example "lock_test"; example "benchmark_old" ;
       example "dining_philosophers"; example "rec_test"; example "three_way"; example "benchmark" ;
       example "two_way"; example "ref_channel"; example "sat"; example "swap_test2";
       example "stack_test_compose"; example "reagent_queue"; example "hw_queue";
       example "eli_stack"; ]
