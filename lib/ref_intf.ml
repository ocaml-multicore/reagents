module type S = sig
  type 'a ref
  (** The type of shared memory reference. *)

  type ('a, 'b) reagent
  (** The type of reagent. *)

  val mk_ref : 'a -> 'a ref
  (** Make a new reference. *)

  val read : 'a ref -> (unit, 'a) reagent
  (** [read r] returns a reagent, which when run returns the value of the reference. *)

  val read_imm : 'a ref -> 'a
  (** [read_imm r] immediately returns the value of the reference. *)

  val cas : ?never_block:bool -> 'a ref -> 'a -> 'a -> (unit, unit) reagent
  (** [cas r e u] returns a reagent, which when run attempts to update the
      reference to [u] if the reference has value [e]. Otherwise, the protocol
      is retried until success. The retry is efficient and does not actively
      consume CPU. The fiber is suspended until there is a possibility of
      success. 
      
      [never_block] is a safety hatch that lets us force failures to be retried.
      It should be never needed when using the [Ref] directly, but might be 
      useful to re-use [Ref] while implementing another reagent. In such a case, a 
      failure that looks permanent at the level of [Ref] can in fact be transient. 

      For example, failure to [push] in Michael-Scott queue is never permanent, 
      instead cas has to be retried on further node. 
      
      *)

  val cas_imm : 'a ref -> 'a -> 'a -> bool
  (** [cas_imm r e u] attempts to atomically update [r] from [e] to [u]. If
      successful, the function returns [true]. Otherwise, returns [false]. *)

  val upd :
    ?never_block:bool ->
    'a ref ->
    ('a -> 'b -> ('a * 'c) option) ->
    ('b, 'c) reagent
  (** [upd r f] returns a reagent value which when run applies [f] to the
      current value [c] of the reference and the input value of the reagent. If
      [f] returns, [None] the protocol is retried. If [f] returns [Some v], the
      reference is attempted to atomically update from [c] to [v]. If the
      update, fails the protocol is retried.

      The retry is efficient and does not actively consume CPU. The fiber is
      suspended until there is a possibility of success. *)
end
