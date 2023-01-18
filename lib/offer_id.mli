type t

val make : int -> t

module Set : sig
  include Set.S with type elt := t
end
