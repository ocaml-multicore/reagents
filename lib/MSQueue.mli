type 'a t
val create    : unit -> 'a t
val push      : 'a t -> 'a -> unit
val pop       : 'a t -> 'a option
val pop_until : 'a t -> ('a -> bool) -> 'a option

type 'a cursor
val snapshot  : 'a t -> 'a cursor
val next      : 'a cursor -> ('a * 'a cursor) option
