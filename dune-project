(lang dune 3.0)
(name reagents)

(generate_opam_files true)

(license BSD-3-clause)
(maintainers "KC Sivaramakrishnan <sk826@cl.cam.ac.uk>")
(authors "KC Sivaramakrishnan <sk826@cl.cam.ac.uk>" )
(source (github ocaml-multicore/reagents))
(documentation "https://ocaml-multicore.github.io/reagents/")

(package
 (name reagents)
 (depends
  (ocaml (>= 5.0))
  (lockfree (>= 0.3.0))
  (kcas (>= 0.2.0))
  (alcotest (and :with-test (>= 1.6.0)))
  (kcas_data (and :with-test (>= 0.2.3)))
  )
 (synopsis "Composable lock-free data and synchronization structures")
 (description "Reagents - Composable lock-free data and synchronization structures"))

