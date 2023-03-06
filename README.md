[API reference](https://ocaml-multicore.github.io/reagents/doc/)

# Reagents — Composable lock-free data and synchronization structures

Reagents are an experimental library for writing multicore programs. Reagents provide an expressive framework for composable multithreading. They support both fine- and coarse-grained multithreading and incorporate mechanisms for efficient retrying.

## Contents

* [Motivation](#motivation)
* [Limitations](#limitiations)
* [Getting Reagents](#getting-reagents)
* [License](#license)
* [Reading](#reading)


## Motivation

Reagents strive to be a comprehensive framework for all things concurrent and parallel. In particular, they have the following advantages over traditional approaches: 

* *Composability*. Operations are trivial to compose with the set of provided composition operators. An item can be moved from one lock-free stack to another in a single atomic lock-free step. Further, a release of one lock can be melded with acquisition of another one. This has far-reaching consequences. For example, a simple way to implement an LRU cache is using a queue and a map, but traditional implementations of thread-safe queue and map do not help as both have to be updated at once. Reagents make that trivial (in contrast with implementing thread-safe LRU cache from scratch) and let us maintain useful properties (in contrast with surrounding both updates with a lock).

* *Expressiveness*. Reagents provide building blocks for various multithreading patterns: communication via sharing memory and message passing, active and passive invocation of operations, conjuction (pair) and disjunction (choice) of operations. 

* *Fine-grained multithreading*. Low-level synchronisation primitives tend to perform and scale better than high-level ones. Reagents use fine-grained multithreading internally and expose them for expert users. 

* *Efficient retrying*. Reagents parametrise over scheduler to suspend and resume fibers as the conditions for their progress are meet or not. This makes common anti-patterns such as busy-waiting easy to avoid.

## Limitiations

Reagents are weaker than transactional memory. A reagent must be decomposable into a list of compare-and-swap operations. This eliminates the need for global serializability of transactions. 

## Getting Reagents

Reagents require OCaml 5.0.0 (`opam switch create 5.0.0`).

Install Reagents from this repository. 
```sh 
git clone git@github.com:ocaml-multicore/reagents.git
cd reagents
opam install . -y
```

## Key concepts

A computation within reagents framework has the following type `('a, 'b) Reagents.t`. Such a value represent a single computation, which takes value of type `'a` and returns value of type `'b`. Internally, it may consist of any number of operations and do any number of side effects. Crucially, regardless of the operations contained within a reagent, all of the execute atomically and entirely or none at all. 

## Sample programs


More sample programs and tests are located in the [`tests`](tests) 
directory of the distribution. They can be built and run with:

    dune build @runtest

Individual tests are built as executables (available in Dune's `_build` directory).

## Internals

## License

Reagents are distributed under ISC license.

## Further Reading

Talks:

* [OCaml multicore and programming with Reagents](https://www.youtube.com/watch?v=qRWTws_YPBA&ab_channel=FunctionalWorks)

Papers in the order of increasing detail:

* [Lock-free programming for the masses](https://kcsrk.info/papers/reagents_ocaml16.pdf)

* [Reagents: Expressing and Composing Fine-grained Concurrency](https://aturon.github.io/academic/reagents.pdf)

* [Understanding and Expressing Scalable Concurrency](https://aturon.github.io/academic/turon-thesis.pdf)
