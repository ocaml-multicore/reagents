[API reference](https://ocaml-multicore.github.io/reagents/doc/)

# Reagents — Composable lock-free data and synchronization structures

Reagents are an experimental library for writing multicore programs. Reagents
provide an expressive framework for composable multithreading. They support both
fine- and coarse-grained multithreading and incorporate mechanisms for efficient
retrying.

## Contents

- [Motivation](#motivation)
- [Limitations](#limitations)
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
  be melded with the acquisition of another one. This has far-reaching
  consequences. For example, a simple way to implement an LRU cache is using a
  queue and a map, but traditional implementations of thread-safe queue and map
  do not help as both have to be updated at once. Reagents make that trivial (in
  contrast with implementing thread-safe LRU cache from scratch) and let us
  maintain useful properties (in contrast with surrounding both updates with a
  lock).

- _Expressiveness_. Reagents provide building blocks for various multithreading
  patterns: communicating by sharing memory and message passing, active and
  passive invocation of operations, conjunction (pair) and disjunction (choice)
  of operations.

- _Fine-grained multithreading_. Low-level synchronisation primitives tend to
  perform and scale better than high-level ones. Reagents use fine-grained
  multithreading internally and expose it for expert users.

- _Efficient retrying_. Reagents parametrise over the scheduler to suspend and
  resume fibers as the conditions for their progress are met or not. This makes
  common anti-patterns such as busy-waiting easy to avoid.

## Limitations

Reagents are weaker than transactional memory. A reagent must be decomposable
into a list of compare-and-swap operations. This eliminates the need for global
serializability of transactions.

## Getting Reagents

Reagents require OCaml 5 (`opam switch create 5.0.0`).

Install Reagents from this repository.

```sh
opam pin -y https://github.com/ocaml-multicore/reagents.git
```

Soon Reagents will be available in the opam repository.

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

This section briefly explains all the key concepts required for using Reagents.

- [Scheduler](#scheduler)
- [Reagent Type](#reagent-type)
- [Combinators](#combinators)
- [Others](#others)
- [Running a Reagent](#running-a-reagent)

### Scheduler

Reagents are parametrised over a minimal
[scheduler interface](lib/scheduler_intf.ml). If an active reagent cannot make
progress, Reagents automatically suspend the fiber. Once someone else updates
the state, Reagents trigger required resumptions. This behavior comes for free.

```ocaml
  type 'a cont

  val suspend : ('a cont -> 'a option) -> 'a
  val resume : 'a cont -> 'a -> unit
  val get_tid : unit -> int
```

A toy scheduler for experimenting and running tests is available in
[Reagents.Toy_scheduler](lib/toy_scheduler.mli).

### Reagent Type

A computation within Reagents framework has the following type
`('a, 'b) Reagents.t`. Here, the computation takes a parameter of type `'a` and
returns a value of type `'b`. Internally, it may consist of any number of
operations and any number of side effects. Crucially, regardless of the
construction of a reagent, all of its operations execute atomically and entirely
or none at all.

### Combinators

Reagents can be composed in arbitrary ways. The following three main combinators
are exposed by the library. These can be composed into more complex combinators.

- ```ocaml
  val (>>>) : ('a,'b) t -> ('b,'c) t -> ('a,'c) t
  ```

Sequential composition runs one reagent after another. It passes the result of
the first one as a parameter to the second one.

- ```ocaml
  val (<*>) : ('a,'b) t -> ('a,'c) t -> ('a,'b * 'c) t
  ```

Conjunction executes both reagents at once and returns both results. Note, this
combinator still attempts to execute its components sequentially. It differs
with [>>>] in information flow only.

- ```ocaml
  val (<+>) : ('a,'b) t -> ('a,'b) t -> ('a,'b) t
  ```

Disjunction tries to execute the first reagent and if it blocks, it attempts the
second one. If both block, the first one to unblock is executed. Also referred
to as a left-biased choice.

### Running a Reagent

Once the desired reagent has been composed, it can be run.

```ocaml
val run : ('a, 'b) t -> 'a -> 'b
```

`run r v` executes the reagent `r` with value `v`.

Note, this function has to be executed from within a scheduler for the
suspension and resumption effects to be handled correctly.

### Others

There are several other values defined in the
[public interface](lib/base_intf.ml) that serve as units, helpers, or
transformations for existing reagents. Perhaps the most notable one is
`attempt`, which converts a blocking reagent into a non-blocking one.

```ocaml
val attempt : ('a, 'b) t -> ('a, 'b option) t
```

`attempt r` converts a blocking reagent into a non-blocking one. If the reagent
blocks, then attempt returns `None`. Otherwise, the reagent is committed
immediately and the returned option value is non-empty.

### Data structures

Reagents expose two core data structures. Complex data structures should utilise
these as building blocks, if possible.

- _Reference_ &mdash; a low-level object akin to an `'a Atomic.t`, which can be
  modified using compare-and-set operation. In contrast with the standard
  library's atomic, if the expected value does not match it is going to suspend
  until the operation can succeed (in the default case).

- _Channel_ &mdash; a two-way channel for sharing memory by communicating.

The library also provides many higher-level data structures (e.g. counter,
stack, queue) and synchronisation primitives (e.g. lock, conditional variable).
See [interface](lib/reagents_intf.ml).

## Sample programs

This section showcases a few applications of Reagents.

- [Counter](#counter)
- [Reference](#reference)
- [Channel](#channel)
- [Catalyst](#catalyst)

### Counter

See a simple example of creating a synchronized counter below.

```ocaml
# let counter = Counter.create 0 in
  let a = run (Counter.inc counter) () in
  let b = run (Counter.inc counter) () in
  let c = run (Counter.dec counter) () in
  (a, b, c);;
- : int * int * int = (0, 1, 2)
```

Both `inc` and `dec` operations are of type `(unit, int) Reagents.t` since they
take a unit as input and return the previous value. Now, imagine there are
several counters representing different statistics about the system, balances of
bank accounts, etc.

Reagents let us take a consistent snapshot of the system without locks.

```ocaml
# let c1 = Counter.create 0 in
  let c2 = Counter.create 0 in

  run (Counter.get c1 <*> Counter.get c2) ();;
- : int * int = (0, 0)
```

### Reference

Continuing from [counter](#counter), we can update any number of locations at
once as well. This example uses references, which are similar to an atomic
variable.

```ocaml
# let account_1 = Ref.mk_ref 100 in
  let account_2 = Ref.mk_ref 0 in

  let transfer a b amount =
    Ref.upd a (fun acc () -> Some (acc - amount, ()))
    >>> Ref.upd b (fun acc () -> Some (acc + amount, ()))
  in

  run (transfer account_1 account_2 100) ();

  ((Ref.read_imm account_1), (Ref.read_imm account_2));;
- : int * int = (0, 100)
```

Note, in the example above the function passed to `Ref.upd` returns an option
type. If the observed value of account is not appropriate for the requested
operation (e.g. the transfer would make account 1 negative), it may choose to
return `None`. In such a case, reagent will block until the value of the
reference is updated by another actor. Alternatively, it can be attempted, to
simply return with failure if the reagent cannot proceed. See
[counter_test.ml](tests/counter_test.ml) for example.

### Channel

Channel is the building block for sharing memory by communication. Reagents
offer a two-way channel (but we can pass units in one direction).

```ocaml
# Scheduler.run (fun () ->
    let endpoint_a, endpoint_b = Channel.mk_chan () in
    Scheduler.fork (fun () -> run (Channel.swap endpoint_a) 12345);
    print_int (run (Channel.swap endpoint_b) ()));;
12345- : unit = ()
```

There are a couple of nuances worth keeping in mind:

- Channels have a blocking nature; the reaction can occur only if there are two
  matching reagents ready to interact. Thus, the one to arrive first is going to
  block until its match is ready.
- Since `<*>` is not truly parallel, there are some limitations to the type of
  channel reactions Reagents are able to commit. See
  [pair_not_parallel.ml](tests/pair_not_parallel.ml) for more details.

### Catalyst

Catalyst is a passively invoked reagent. It does not react on its own, instead,
it remains ready to react with others as many times as needed until canceled.
Catalysts let us link multiple data structures to form a graph of computations.
See [catalyst_test.ml](tests/catalyst_test.ml) for examples of linking channels.

### More

More sample programs and tests are located in the [`tests`](tests) directory of
the distribution. They can be built and run with:

    dune build @runtest

Individual tests are built as executables (available in Dune's `_build`
directory).

## Development

### Formatting

This project uses [ocamlformat](https://github.com/ocaml-ppx/ocamlformat) (for
OCaml) and [prettier](https://prettier.io/) (for Markdown).

### Internals quick start

Reagents are largely driven by [kcas](https://github.com/ocaml-multicore/kcas).
kcas is a software solution for executing multiple atomic operations as a single
transaction on architectures providing a single-word CAS only. The current
implementation of kcas requires k+1 atomic operations for k-location update.

In the non-blocking case, Reagents constitute a convenient abstraction over the
specification and aggregation of individual atomic operations. If the list of
required atomic operations can be constructed and committed immediately, a
reagent succeeds using the fast-path.

However, an operation may be unable to proceed. If fast-path found that the
operation cannot finish (e.g. pop on an empty stack), Reagents core generates an
offer. The offer is then published in a relevant queue with extra information
and fiber suspends on it. Once another thread comes, it sees the offer and
resumes fibers that are now unblocked. This logic is reagent-specific. In the
case of reference, it's going to wake up all waiters. In the case of channel, it
will take suspended thread's transaction, merge it with its own, and try to
commit everything at once. If the commit succeeds, it provides the suspended
thread with the result and resumes it. Both actions cancel the offer.

These two mechanisms are the key design choices behind Reagents.

## License

Reagents are distributed under ISC license.

## Further Reading

Talks:

- [OCaml multicore and programming with Reagents](https://www.youtube.com/watch?v=qRWTws_YPBA&ab_channel=FunctionalWorks)

Papers in the order of increasing detail:

- [Lock-free programming for the masses](https://kcsrk.info/papers/reagents_ocaml16.pdf)

- [Reagents: Expressing and Composing Fine-grained Concurrency](https://aturon.github.io/academic/reagents.pdf)

- [Understanding and Expressing Scalable Concurrency](https://aturon.github.io/academic/turon-thesis.pdf)
