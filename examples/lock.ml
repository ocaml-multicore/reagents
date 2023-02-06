type 'a holder = 'a option ref
type 'a t = 'a holder Atomic.t

let make initial = Atomic.make (ref (Some initial))

let rec wait prev =
  match !prev with
  | None ->
      Domain.cpu_relax ();
      wait prev
  | Some value -> value

let acquire m =
  let next = ref None in
  (next, wait @@ Atomic.exchange m next)

let release holder value =
  assert (None == !holder);
  holder := Some value
  [@@inline]
