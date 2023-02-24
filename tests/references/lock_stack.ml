(*
 * Copyright (c) 2015, Théo Laurent <theo.laurent@ens.fr>
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

type 'a t = 'a list ref * bool Atomic.t

let max_iters = 100000

let rec lock m = function
  | 0 ->
      ignore (Unix.select [] [] [] 0.1);
      lock m max_iters
  | n -> if Atomic.compare_and_set m false true then () else lock m (n - 1)

let lock m = lock m max_iters
let rec unlock m = if Atomic.compare_and_set m true false then () else unlock m
let create () : 'a t = (ref [], Atomic.make false)

let push (l, m) v =
  lock m;
  l := v :: !l;
  unlock m

let pop (l, m) =
  lock m;
  let r =
    match !l with
    | [] -> None
    | x :: xl ->
        l := xl;
        Some x
  in
  unlock m;
  r
