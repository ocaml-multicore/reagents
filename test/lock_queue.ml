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

type 'a t = ('a list * 'a list) ref * bool Cas.ref

let max_iters = 100000

let rec lock m = function
  | 0 -> 
      begin
        ignore (Unix.select [] [] [] 0.1);
        lock m max_iters
      end
  | n -> 
      if Cas.cas m false true then ()
      else lock m (n - 1)

let lock m = lock m max_iters

let rec unlock m =
  if Cas.cas m true false then ()
  else unlock m

let create () = (ref ([], []), Cas.ref false)

let push (q,m) v =
  lock m;
  let (front,back) = !q in
  q := (front,v::back);
  unlock m

let pop (q,m) =
  lock m;
  let (front, back) = !q in
  let r =
    match front with
    | x::xs ->
        ( q := (xs,back);
          Some x )
    | [] ->
        begin
          match back with
          | [] -> None
          | xs ->
              begin
                match List.rev xs with
                | [] -> failwith "impossible"
                | x::xs -> ( q := (xs,[]); Some x)
              end
        end
  in
  unlock m; r
