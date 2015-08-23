TEST=test/swap_test.byte test/ref_test.byte test/counter_test.byte
FLAGS=-Is lib,data -lib unix

all: reagents data tests

reagents:
	ocamlbuild $(FLAGS) Reagents.cma

data: reagents
	ocamlbuild $(FLAGS) Reagents_data.cma

tests:
	ocamlbuild $(FLAGS) $(TEST)

clean:
	ocamlbuild -clean
