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

module type S = Reaction_intf.S

module Make (Sched : Scheduler.S) : S = struct
  type t = {
    cases : PostCommitCas.t list;
    offers : Offer_id.Set.t;
    post_commits : (unit -> unit) list;
  }

  let empty = { cases = []; offers = Offer_id.Set.empty; post_commits = [] }
  let has_offer { offers; _ } offer_id = Offer_id.Set.mem offer_id offers
  let with_CAS r cas = { r with cases = cas :: r.cases }
  let with_post_commit r pc = { r with post_commits = pc :: r.post_commits }

  let with_offer r offer_id =
    { r with offers = Offer_id.Set.add offer_id r.offers }

  let cas_count r = List.length r.cases

  let union r1 r2 =
    {
      cases = r1.cases @ r2.cases;
      offers = Offer_id.Set.union r1.offers r2.offers;
      post_commits = r1.post_commits @ r2.post_commits;
    }

  let try_commit r =
    let do_post_commit r = function
      | None -> false
      | Some pc ->
          pc ();
          List.iter (fun f -> f ()) r.post_commits;
          true
    in
    match r.cases with
    | [] -> true
    | [ cas ] -> do_post_commit r @@ PostCommitCas.commit cas
    | l -> do_post_commit r @@ PostCommitCas.kCAS l
end
