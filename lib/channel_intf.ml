module type S = sig
  type ('a, 'b) endpoint
  (** The type of endpoint which accepts value of type ['a] and return value of
      type ['b]. *)

  type ('a, 'b) reagent
  (** The type of reagent. See {!Reagents.S.t}. *)

  val mk_chan : ?name:string -> unit -> ('a, 'b) endpoint * ('b, 'a) endpoint
  (** Make a new channel. Returns a pair of dual endpoints. *)

  val swap : ('a, 'b) endpoint -> ('a, 'b) reagent
  (** Swap on the channel. *)
end
