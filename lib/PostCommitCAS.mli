type 'a ref = 'a CAS.ref
val ref : 'a -> 'a ref
val get : 'a ref -> 'a

type t
val return    : bool -> (unit -> unit) -> t
val cas       : 'a ref -> 'a -> 'a -> (unit -> unit) -> t
val is_on_ref : t -> 'a ref -> bool
val commit    : t -> (unit -> unit) option
val kCAS      : t list -> (unit -> unit) option
