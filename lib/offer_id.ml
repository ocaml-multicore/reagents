type t = int

let make v = v

module Set = Set.Make (struct
  type t = int

  let compare = compare
end)
