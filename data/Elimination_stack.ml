module type S = sig
  type 'a t
  type ('a,'b) reagent
  val create  : unit -> 'a t
  val push    : 'a t -> ('a, unit) reagent
  val pop     : 'a t -> (unit, 'a) reagent
  val try_pop : 'a t -> (unit, 'a option) reagent
end

module Make (Reagents: Reagents.S) : S
  with type ('a,'b) reagent = ('a,'b) Reagents.t = struct

  type ('a,'b) reagent = ('a,'b) Reagents.t

  module TS = Treiber_stack.Make(Reagents)
  module C = Reagents.Channel
  open Reagents

  type 'a t =
    {stack     : 'a TS.t;
     elim_push : ('a,unit) C.endpoint;
     elim_pop  : (unit,'a) C.endpoint}

  let create () =
    let (elim_push, elim_pop) = C.mk_chan () in
    { stack = TS.create (); elim_push; elim_pop }

  let push r = TS.push r.stack <+> C.swap r.elim_push

  let pop r = TS.pop r.stack <+> C.swap r.elim_pop

  let try_pop r =
    let side_chan = C.swap r.elim_pop >>= (fun x -> constant (Some x)) in
    TS.try_pop r.stack <+> side_chan

end
