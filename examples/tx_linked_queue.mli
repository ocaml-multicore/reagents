open Kcas

type 'a t

val create : unit -> 'a t
val push : 'a t -> 'a -> unit Tx.t
val pop : 'a t -> 'a option Tx.t
