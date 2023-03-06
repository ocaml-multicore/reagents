(** Toy scheduler to run tests and have something working in utop. 
    Do not use in actual applications.
*)

exception All_domains_idle

val make : ?raise_if_all_idle:bool -> int -> unit -> (module Toy_scheduler_intf.S)
  (** [make k ()] instantiates a scheduler with k domains in the pool.

    [raise_if_all_idle] controls whether the scheduler is going to raise
    [All_domains_idle] when it runs out of tasks, while the initial task 
    is suspended. It may raise spuriously in multi-domain setting, thus 
    it is turned off by default.
  *)
