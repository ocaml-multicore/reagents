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

module type S = Reagents_intf.S

module Make (Sched : Scheduler.S) : S = struct
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
