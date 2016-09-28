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

type 'a ref

val ref : 'a -> 'a ref

val get : 'a ref -> 'a

val get_id : 'a ref -> int

type t

val mk_cas : 'a ref -> 'a -> 'a -> t

val cas : 'a ref -> 'a -> 'a -> bool

val is_on_ref : t -> 'a ref -> bool

val commit : t -> bool

val kCAS : t list -> bool

type 'a cas_result = Aborted | Failed | Success of 'a

val try_map : 'a ref -> ('a -> 'a option) -> 'a cas_result

val map : 'a ref -> ('a -> 'a option) -> 'a cas_result

val incr : int ref -> unit

val decr : int ref -> unit

val print_stats : int -> unit
