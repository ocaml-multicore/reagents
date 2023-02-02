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

module type Scheduler = sig
  type 'a cont

  val suspend : ('a cont -> 'a option) -> 'a
  val resume : 'a cont -> 'a -> unit
  val get_tid : unit -> int
end

module type S = sig
  type ('a, 'b) t

  val never : ('a, 'b) t
  val constant : 'a -> ('b, 'a) t
  val post_commit : ('a -> unit) -> ('a, 'a) t
  val lift : ('a -> 'b) -> ('a, 'b) t
  val lift_blocking : ('a -> 'b option) -> ('a, 'b) t
  val return : ('a -> (unit, 'b) t) -> ('a, 'b) t
  val ( >>= ) : ('a, 'b) t -> ('b -> (unit, 'c) t) -> ('a, 'c) t
  val ( >>> ) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  val ( <+> ) : ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
  val ( <*> ) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t
  val attempt : ('a, 'b) t -> ('a, 'b option) t
  val run : ('a, 'b) t -> 'a -> 'b

  type catalyst

  val catalyse : ('a, 'b) t -> 'a -> catalyst
  val cancel_catalyst : catalyst -> unit

  module Ref : Ref.S with type ('a, 'b) reagent = ('a, 'b) t
  module Channel : Channel.S with type ('a, 'b) reagent = ('a, 'b) t

  module Data : sig
    module Counter : Counter.S with type ('a, 'b) reagent = ('a, 'b) t

    module Treiber_stack :
      Treiber_stack.S with type ('a, 'b) reagent = ('a, 'b) t

    module Elimination_stack :
      Elimination_stack.S with type ('a, 'b) reagent = ('a, 'b) t

    module MichaelScott_queue :
      MichaelScott_queue.S with type ('a, 'b) reagent = ('a, 'b) t
  end

  module Sync : sig
    module Countdown_latch :
      Countdown_latch.S with type ('a, 'b) reagent = ('a, 'b) t

    module Exchanger : Exchanger.S with type ('a, 'b) reagent = ('a, 'b) t
    module Lock : Lock.S with type ('a, 'b) reagent = ('a, 'b) t

    module Recursive_lock (Tid : sig
      val get_tid : unit -> int
    end) : Recursive_lock.S with type ('a, 'b) reagent = ('a, 'b) t

    module Condition_variable :
      Condition_variable.S
        with type ('a, 'b) reagent = ('a, 'b) t
         and type lock = Lock.t
  end
end

module Make (Sched : Scheduler) : S = struct
  module B = struct
    include Core.Make (Sched)
    module Ref = Ref.Make (Sched)
    module Channel = Channel.Make (Sched)
  end

  include B

  module Data = struct
    module Counter = Counter.Make (B)
    module Treiber_stack = Treiber_stack.Make (B)
    module Elimination_stack = Elimination_stack.Make (B)
    module MichaelScott_queue = MichaelScott_queue.Make (B)
  end

  module Sync = struct
    module Countdown_latch = Countdown_latch.Make (B)
    module Exchanger = Exchanger.Make (B)
    module Lock = Lock.Make (B)
    module Recursive_lock = Recursive_lock.Make (B)
    module Condition_variable = Condition_variable.Make (B) (Lock)
  end
end
