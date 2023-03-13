(*
 * Copyright (c) 2015, KC Sivaramakrishnan <sk826@cl.cam.ac.uk>
 *
 * Permission to use, copy, modify, and/or distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

module Scheduler = (val Reagents.Toy_scheduler.make 1 ())
module Reagents = Reagents.Make (Scheduler)
open Reagents

let update () =
  Scheduler.run (fun () ->
      let r = Ref.mk_ref 0 in
      let foo ov () = if ov = 0 then None else Some (2, ()) in
      Scheduler.fork (fun () -> run (Ref.upd r foo) ());
      run (Ref.cas r 0 1) ())

let update_monadic () =
  Scheduler.run (fun () ->
      let r = Ref.mk_ref 2 in
      let test2_rg =
        Ref.read r >>= fun i -> if i <> 3 then never else constant ()
      in
      Scheduler.fork (run test2_rg);
      run (Ref.cas r 2 3) ())

let () =
  let open Alcotest in
  run "ref test"
    [
      ( "simple",
        [
          test_case "upd" `Quick update;
          test_case "monadic upd" `Quick update_monadic;
        ] );
    ]
