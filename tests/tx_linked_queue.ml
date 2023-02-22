open Kcas

type 'a t = { front : 'a node Loc.t; back : 'a node Loc.t }
and 'a node = Nil | Node of 'a node Loc.t * 'a

let create () = { front = Loc.make Nil; back = Loc.make Nil }

let push queue value =
  Tx.(
    delay @@ fun () ->
    let node = Node (Loc.make Nil, value) in
    exchange queue.back node >>= function
    | Nil -> set queue.front node
    | Node (next, _) -> set next node)

let pop queue =
  Tx.(
    get queue.front >>= function
    | Nil -> return None
    | Node (next, value) -> (
        get next >>= function
        | Nil -> set queue.front Nil >> set queue.back Nil >>. Some value
        | node -> set queue.front node >>. Some value))
