TEST=test/swap_test.byte test/ref_test.byte
FLAGS=-Is lib -lib unix

all: reagents tests

reagents:
	ocamlbuild $(FLAGS) Reagents.cma

tests:
	ocamlbuild $(FLAGS) $(TEST)

clean:
	ocamlbuild -clean
