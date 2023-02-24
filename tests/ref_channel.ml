module type REF_CHANNEL = sig
  type ('a, 'b) reagent
  type 'a channel

  val mk_chan : unit -> 'a channel
  val send : 'a channel -> ('a, unit) reagent
  val recv : 'a channel -> (unit, 'a) reagent
end

module Ref_channel (Reagents : Reagents.S) :
  REF_CHANNEL with type ('a, 'b) reagent = ('a, 'b) Reagents.t = struct
  type ('a, 'b) reagent = ('a, 'b) Reagents.t

  open Reagents

  type 'a channel = 'a option Ref.ref

  let mk_chan () = Ref.mk_ref None

  let send r =
    Ref.upd r (fun st v ->
        match st with None -> Some (Some v, ()) | _ -> None)

  let recv r =
    Ref.upd r (fun st _ ->
        match st with None -> None | Some v -> Some (None, v))
end

module Scheduler = (val Sched_ws.make 1 ())
module Reagents = Reagents.Make (Scheduler)
open Reagents
module Channel = Ref_channel (Reagents)

let chan_send_receive () =
  Scheduler.run (fun () ->
      let c = Channel.mk_chan () in
      Scheduler.fork (fun () -> Printf.printf "%d\n" (run (Channel.recv c) ()));
      run (Channel.send c) 10)

let () =
  let open Alcotest in
  run "ref channel"
    [ ("simple", [ test_case "send receive" `Quick chan_send_receive ]) ]
