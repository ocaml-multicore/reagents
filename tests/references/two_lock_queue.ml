type 'a t = { front : 'a list Lock.t; back : 'a list Lock.t }

let create () : 'a t = { front = Lock.make []; back = Lock.make [] }

let push q v =
  let hold, back = Lock.acquire q.back in
  Lock.release hold (v :: back)

let pop q =
  let hold_of_front, front = Lock.acquire q.front in
  match front with
  | x :: xs ->
      Lock.release hold_of_front xs;
      Some x
  | [] -> (
      let hold_of_back, back = Lock.acquire q.back in
      Lock.release hold_of_back [];
      match back with
      | [] ->
          Lock.release hold_of_front [];
          None
      | xs -> (
          match List.rev xs with
          | [] -> failwith "impossible"
          | x :: xs ->
              Lock.release hold_of_front xs;
              Some x))
