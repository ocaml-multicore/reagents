open CAS

let print_usage_and_exit () =
  print_endline @@ "Usage: " ^ Sys.argv.(0) ^ " <num_domains> <num_iters> <cas_length>";
  exit(0)

let (num_doms, num_iters, cas_length) =
  if Array.length Sys.argv < 3 then
    print_usage_and_exit ()
  else
    try
      let a = int_of_string (Sys.argv.(1)) in
      let b = int_of_string (Sys.argv.(2)) in
      let c = int_of_string (Sys.argv.(3)) in
      (a,b,c)
    with
    | Failure _ -> print_usage_and_exit ()

let mk_arr () = [| ref 0; ref 0; ref 0; ref 0; ref 0; ref 0; ref 0; ref 0 |]
let arr = Array.init 1024 (fun _ -> mk_arr ())

let finish_arr = Array.init num_doms (fun _ -> 0)

let rec wait_till_done () =
  let sum = Array.fold_left (fun acc i -> acc + i) 0 finish_arr in
  if sum = num_doms then ()
  else
    (ignore @@ Unix.select [] [] [] 0.01;
     wait_till_done ())

let verify () =
  let sum = Array.fold_left (fun acc a ->
        acc + Array.fold_left (fun acc r -> acc + CAS.get r) 0 a) 0 arr
  in
  if not (sum = num_doms * num_iters * cas_length) then
    Printf.printf "sum=%d expected=%d\n%!" sum (num_doms * num_iters * cas_length);
  assert (sum = num_doms * num_iters * cas_length)

let core () =
  let dom_id = Domain.self () in
  Random.self_init ();
  (* print_string @@ Printf.sprintf "[%d] Initing...\n" dom_id; *)
  for i = 0 to num_iters - 1
  do
    let rec prep acc = function
      | 0 -> acc
      | n ->
        let (a,b) = Random.int 1024, Random.int 8 in
        let v = CAS.get arr.(a).(b) in
        prep ((cas arr.(a).(b) {expect = v; update = v+1})::acc) (n-1)
    in
    let rec loop () =
      let res =
        match prep [] cas_length with
        | [c] -> commit c
        | l -> kCAS l
      in
      if not res then loop ()
    in
    loop ()
  done;
  (* print_string @@ Printf.sprintf "[%d] Exit\n" dom_id; *)
  finish_arr.(dom_id) <- 1

let _ =
  for i = 1 to num_doms - 1
  do Domain.spawn core done

(* let _ = Pervasives.at_exit (fun () -> CAS.print_stats num_doms) *)
let _ = core ()
let _ = wait_till_done ()
let _ = verify ()
