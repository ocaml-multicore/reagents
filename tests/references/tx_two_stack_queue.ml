open Kcas

type 'a t = { front : 'a list Loc.t; back : 'a list Loc.t }

let create () = { front = Loc.make []; back = Loc.make [] }
let push q x = Tx.modify q.front @@ List.cons x

let pop q =
  Tx.(
    get q.front >>= function
    | x :: xs -> set q.front xs >>. Some x
    | [] -> (
        get_as List.rev q.back >>= function
        | [] -> return None
        | x :: xs -> set q.back [] >> set q.front xs >>. Some x))
