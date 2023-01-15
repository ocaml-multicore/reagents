module Scheduler = Sched_ws.Make (struct
  let num_domains = 1
  let is_affine = false
  let work_stealing = false
end)

module Reagents = Reagents.Make (Scheduler)
module R_data = Reagents.Data
module Counter = R_data.Counter
open Reagents


let main () =
  let c = Counter.create 0 in
  Printf.printf "%d\n%!" @@ run (Reagents.change_commit (Counter.get c)) ();
  ()
;;

let () = Scheduler.run_allow_deadlock main
