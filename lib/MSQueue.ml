(*
 * Copyright (c) 2015, Théo Laurent <theo.laurent@ens.fr>
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

(* Michael-Scott queue *)

(* TODO KC: Replace with concurrent lock free bag --
 * http://dl.acm.org/citation.cfm?id=1989550 *)

open CAS.Sugar

type 'a node =
  | Nil
  | Next of 'a * 'a node CAS.ref

type 'a t =
  { head : 'a node CAS.ref ;
    tail : 'a node CAS.ref }

let create () =
  let head = (Next (Obj.magic (), ref Nil)) in
  { head = ref head ; tail = ref head }

let is_empty q =
  match !(q.head) with
  | Nil -> failwith "MSQueue.is_empty: impossible"
  | Next (_,x) ->
      ( match !x with
        | Nil -> true
        | _ -> false )

let pop q =
  let b = Backoff.create () in
  let rec loop () =
    let s = !(q.head) in
    let nhead = match s with
      | Nil -> failwith "MSQueue.pop: impossible"
      | Next (_, x) -> !x
    in match nhead with
     | Nil -> None
     | Next (v, _) when (q.head <!= s --> nhead) -> Some v
     | _ -> ( Backoff.once b ; loop () )
  in loop ()

let push q v =
  let rec find_tail_and_enq curr_end node =
    if curr_end <!= (Nil --> node) then ()
    else match !curr_end with
         | Nil -> find_tail_and_enq curr_end node
         | Next (_, n) -> find_tail_and_enq n node
  in
  let newnode = Next (v, ref Nil) in
  let tail = !(q.tail) in
  match tail with
  | Nil         -> failwith "HW_MSQueue.push: impossible"
  | Next (_, n) ->
      ( find_tail_and_enq n newnode ;
        ignore (q.tail <!= tail --> newnode) )

let rec clean_until q f =
  let b = Backoff.create () in
  let rec loop () =
    let s = !(q.head) in
    let nhead = match s with
      | Nil -> failwith "MSQueue.pop: impossible"
      | Next (_, x) -> !x
    in match nhead with
     | Nil -> ()
     | Next (v, _) ->
         if not (f v) then
            if (q.head <!= s --> nhead)
            then (Backoff.reset b; loop ())
            else (Backoff.once b; loop ())
         else ()
  in loop ()

type 'a cursor = 'a node

let snapshot q =
  match !(q.head) with
  | Nil -> failwith "MSQueue.snapshot: impossible"
  | Next (_, n) -> !n

let next c =
  match c with
  | Nil -> None
  | Next (a, n) -> Some (a, !n)
