Reagents strive to be more expressive than pure kcas, while maintaining
lock-freedom. In particular, they have a richer support for blocking:

- Reagents always wait efficiently.
- They support complex waiting patterns, e.g. "rendezvous" - two domains
  exchanging items atomically.

However, existing interface overpromises on the actual capabilities. Most
importantly, the rendezvous cases do not compose. While some access patterns
involving multiple domains and multiple communication objects work fine, a
slight tweak may make a reagent deadlock. For working use-cases see
[tests/swap_test.ml](tests/swap_test.ml). Attempt to fix this behaviour leads to
a deeper underlying issue - the parallel composition.

The parallel composition is implemented as syntactic sugar over sequential
composition, which means there is no true parallelism underneath. Suppose
`lhs <*> rhs`. If lhs reagent depends on rhs reagent, the composition is going
to deadlock. While a direct dependency between the two does not make sense,
there are real-world programs, where such a dependency arises transitively
through interaction with another reagent. See
[test/pair_not_parallel.ml](tests/pair_not_parallel.ml) for examples. Fixing
this behaviour is not trivial
([issue](https://github.com/ocaml-multicore/reagents/issues/16)) and as far as
the literature is concerned, such a truly parallel composition in the context of
reagents would be a novelty.

By comparison, kcas offers a more limited support for blocking, which remains
useful for real-world programs and does not suffer from edge cases described
above. Furthermore, the thinner abstraction layer over core kcas makes it more
susceptible to optimization and, at the moment, kcas performs significantly
better.
