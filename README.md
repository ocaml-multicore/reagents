[API reference](https://ocaml-multicore.github.io/reagents/doc/)

# Reagents — Composable lock-free data and synchronization structures

Reagents are an experimental library for writing multicore programs. Reagents
provide an expressive framework for composable multithreading. They support both
fine- and coarse-grained multithreading and incorporate mechanisms for efficient
retrying.

## Contents

- [Motivation](#motivation)
- [Limitations](#limitiations)
- [Getting Reagents](#getting-reagents)
- [Key Concepts](#key-concepts)
- [Development](#development)
- [License](#license)
- [Reading](#reading)

## Motivation

Reagents strive to be a comprehensive framework for all things concurrent and
parallel. In particular, they have the following advantages over traditional
approaches:

- _Composability_. Operations are trivial to compose with the set of provided
  composition operators. An item can be moved from one lock-free stack to
  another in a single atomic lock-free step. Further, a release of one lock can
  be melded with acquisition of another one. This has far-reaching consequences.
  For example, a simple way to implement an LRU cache is using a queue and a
  map, but traditional implementations of thread-safe queue and map do not help
  as both have to be updated at once. Reagents make that trivial (in contrast
  with implementing thread-safe LRU cache from scratch) and let us maintain
  useful properties (in contrast with surrounding both updates with a lock).

- _Expressiveness_. Reagents provide building blocks for various multithreading
  patterns: communication via sharing memory and message passing, active and
  passive invocation of operations, conjuction (pair) and disjunction (choice)
  of operations.

- _Fine-grained multithreading_. Low-level synchronisation primitives tend to
  perform and scale better than high-level ones. Reagents use fine-grained
  multithreading internally and expose them for expert users.

- _Efficient retrying_. Reagents parametrise over scheduler to suspend and
  resume fibers as the conditions for their progress are meet or not. This makes
  common anti-patterns such as busy-waiting easy to avoid.

## Limitiations

Reagents are weaker than transactional memory. A reagent must be decomposable
into a list of compare-and-swap operations. This eliminates the need for global
serializability of transactions.

## Getting Reagents

Reagents require OCaml 5.0.0 (`opam switch create 5.0.0`).

Install Reagents from this repository.

```sh
git clone git@github.com:ocaml-multicore/reagents.git
cd reagents
opam install . -y
```

Test the setup in utop with the following snippet.

```ocaml
# #require "reagents";;

# module Scheduler = (val Reagents.Toy_scheduler.make 1 ())
  open Reagents.Make (Scheduler);;

# let s = Ref.mk_ref "hello world\n" in
  Scheduler.run (run (Ref.read s >>> lift print_string));;

hello world
- : unit = ()
```

## Key concepts

### Scheduler

Reagents are parametrised over a minimal
[scheduler interface](lib/scheduler_intf.ml) to avoid busy-waiting.

```ocaml
  type 'a cont

  val suspend : ('a cont -> 'a option) -> 'a
  val resume : 'a cont -> 'a -> unit
  val get_tid : unit -> int
```

If a running reagent cannot make progress, the fiber is automatically suspended.
Once someone else updates relevant state, Reagents trigger required resumptions.
A toy scheduler is available in [Reagents.Toy_scheduler](lib/toy_scheduler.mli).

### Type

A computation within reagents framework has the following type
`('a, 'b) Reagents.t`. Here, the computation takes a parameter of type `'a` and
returns value of type `'b`. Internally, it may consist of any number of
operations and do any number of side effects. Crucially, regardless of the
construction of a reagent, all of its operations execute atomically and entirely
or none at all.

### Combinators

Reagents can be composed in arbitrary ways. The following three main combinators
are exposed by the library.

```ocaml
(* Sequential composition runs one reagent after another. It passes the result of the first one as parameter to the second one. *)
val (>>>) : ('a,'b) t -> ('b,'c) t -> ('a,'c) t
(* Conjunction executes both reagents at once and returns both results. Note, this combinator still attempts to execute its components sequentially. It only differs with [>>>] in information flow. *)
val (<*>) : ('a,'b) t -> ('a,'c) t -> ('a,'b * 'c) t
(* Disjunction tries to execute the first reagent and if it blocks, it attempts the second one. If both block, the first one to unblock is executed. *)
val (<+>) : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
```

### Others

There is a number of other values defined in the
[public interface](lib/base_intf.ml) that serve as units, helpers or
transformations for existing reagents. Perhaps the most notable one is
`attempt`.

```ocaml
val attempt : ('a, 'b) t -> ('a, 'b option) t
(** Convert a blocking reagent into a non-blocking one. If reagent [r] is a
    blocks, then [attempt r] return [None]. If [r] does not block and returns
    a value [v], then [attempt r] returns [Some v]. *)
```

### Executing a reagent

Once a desired reagent has been composed, it can be run.

```ocaml
val run : ('a, 'b) t -> 'a -> 'b
(** [run r v] runs the reagents [r] with value [v]. *)

```

Note, this function has to be executed from within a scheduler for the
suspension and resumption effects to be handled correctly.

### Data structures

Reagents exposes two core data structures. Complex data structures should
utilise these as buildling blocks, if possible.

- Reference - a low-level object akin to an `'a Atomic.t`, which can be modified
  using compare-and-set operation. In contrast with standard library's atomic,
  if the expected value does not match it is going to suspend until operation
  can succeed (in the default case).

- Channel - a two-way channel for sharing memory by communicating.

The library also provides a number of higher level data structures and
synchronisation primitives. See [intf](lib/reagents_intf.ml).

## Sample programs

Sample programs and tests are located in the [`tests`](tests) directory of the
distribution. They can be built and run with:

    dune build @runtest

Individual tests are built as executables (available in Dune's `_build`
directory).

## Development

### Formatting

This project uses [ocamlformat](https://github.com/ocaml-ppx/ocamlformat) (for
OCaml) and [prettier](https://prettier.io/) (for Markdown).

### To make a new release

1. Update [CHANGES.md](CHANGES.md).
2. Run `dune-release tag VERSION` to create a tag for the new `VERSION`.
3. Run `dune-release` to publish the new `VERSION`.

### Internals quick start

Reagents are largely driven by [kcas](https://github.com/ocaml-multicore/kcas).
kcas is a software solution for executing multiple atomic (CAS / get) operations
as a single transaction on architectures providing only a single-word CAS. The
current implementation of kcas requires only k+1 atomic operations for
k-location update.

In the non-blocking case, Reagents constitute a convenient abstraction over
specification and aggregation of individual atomic operations. If the atomic
operations can be constructed and committed immediately, a reagent succeeds
using the fast-path.

However, an operation may be unable to proceed. If fast-path found that
operation cannot finish (e.g. pop on empty stack), Reagents core generates an
offer, which is published in relevant queue with extra information and fiber
suspends on it. Then, when another thread comes, it sees the offer and acts
accordingly. In the case of reference, it's going to wake-up all waiters. In the
case of channel, it will take suspended thread's transaction, merge it with
their own, and try to commit everything at once. If the commit succeeds, it
provides suspended thread with the result and resumes it. That cancels the
offer.

These two mechanisms are fundamental to the design of Reagents.

## License

Reagents are distributed under ISC license.

## Further Reading

Talks:

- [OCaml multicore and programming with Reagents](https://www.youtube.com/watch?v=qRWTws_YPBA&ab_channel=FunctionalWorks)

Papers in the order of increasing detail:

- [Lock-free programming for the masses](https://kcsrk.info/papers/reagents_ocaml16.pdf)

- [Reagents: Expressing and Composing Fine-grained Concurrency](https://aturon.github.io/academic/reagents.pdf)

- [Understanding and Expressing Scalable Concurrency](https://aturon.github.io/academic/turon-thesis.pdf)
