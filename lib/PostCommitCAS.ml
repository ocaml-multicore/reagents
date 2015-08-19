type 'a ref = 'a CAS.ref
let ref = CAS.ref
let get = CAS.get

type cas_kind = Real of CAS.t | Imm of bool

type t = cas_kind * (unit -> unit)

let cas r old_v new_v post_commit = (Real (CAS.cas r {expect = old_v; update = new_v}), post_commit)

let is_on_ref (c, _) r =
  match c with
  | Real cas -> CAS.is_on_ref cas r
  | _ -> false

let commit (cas, post_commit) =
  match cas with
  | Real cas -> if CAS.commit cas then Some post_commit else None
  | Imm v -> if v then Some post_commit else None

let return v post_commit = (Imm v, post_commit)

let (>>) = fun f g x -> f(g(x))

let kCAS lst =
  let (cas_list, post_commit, live) =
    List.fold_left (fun (l1,l2,live) (cas,pc) ->
      match cas with
      | Real c -> (c::l1, pc >> l2, live)
      | Imm v -> (l1, pc >> l2, v && live)) ([],(fun () -> ()), true) lst
  in
  if live && CAS.kCAS cas_list
  then Some (post_commit)
  else None
