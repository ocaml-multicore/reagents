module type REF_CHANNEL = sig
  type ('a,'b) reagent
  type 'a channel
  val mk_chan : unit -> 'a channel
  val send : 'a channel -> ('a,unit) reagent
  val recv : 'a channel -> (unit,'a) reagent
end

module Ref_channel(Reagents : Reagents.S) 
  : REF_CHANNEL with type ('a,'b) reagent = ('a,'b) Reagents.t = struct

  type ('a,'b) reagent = ('a,'b) Reagents.t

  open Reagents

  type 'a channel = 'a option Ref.ref

  let mk_chan () = Ref.mk_ref None

  let send r =
    Ref.upd r (fun st v -> 
      match st with
      | None -> Some (Some v, ())
      | _ -> None)

  let recv r = 
    Ref.upd r (fun st v ->
      match st with
      | None -> None
      | Some v -> Some (None, v))
 end

open Printf
module Scheduler = Sched_ws.Make(struct let num_domains = 1 end)
module Reagents = Reagents.Make (Scheduler)
open Scheduler
open Reagents
module Channel = Ref_channel(Reagents)
open Channel

let id_str () = sprintf "%d:%d" (Domain.self ()) (get_tid ())

let main () = 
  let c = mk_chan () in
  fork (fun () -> Printf.printf "%d\n" (run (recv c) ()));
  run (send c) 10

let () = Scheduler.run main
