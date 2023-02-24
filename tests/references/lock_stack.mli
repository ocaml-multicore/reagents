type 'a t = 'a list ref * bool Atomic.t

val max_iters : int
val lock : bool Atomic.t -> unit
val unlock : bool Atomic.t -> unit
val create : unit -> 'a t
val push : 'a list ref * bool Atomic.t -> 'a -> unit
val pop : 'a list ref * bool Atomic.t -> 'a option
