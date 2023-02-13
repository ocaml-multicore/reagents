(** Scalable lock mechanism based on the CLH algorithm. *)

type 'a t
(** Represents a lock that protects an immutable value of type ['a]. *)

type 'a holder
(** Represents an exclusive hold of an immutable value of type ['a] protected by
    an associated lock. *)

val make : 'a -> 'a t
(** [make value] creates a new lock protecting the given immutable [value]. *)

val acquire : 'a t -> 'a holder * 'a
(** [acquire lock] acquires the lock and returns a pair of a {!holder} and the
    protected immutable value. *)

val release : 'a holder -> 'a -> unit
(** [release hold value] releases the lock and allows the next {!acquire} in
    queue to proceed with the given immutable [value].

    WARNING: [release] does not perform an atomic release fence.  This means
    that non-initialising stores performed inside the critical section between
    {!acquire} and [release] may not be published before the [value] is seen by
    the next {!acquire}. *)
