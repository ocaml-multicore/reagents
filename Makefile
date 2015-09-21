TEST=test/swap_test.byte test/ref_test.byte test/counter_test.byte test/queue_test.byte test/stack_test.byte test/lock_test.byte
FLAGS=-cflag -annot -cflag -g -lflag -g -Is lib,data,sync -lib unix

all: reagents data sync tests

reagents:
	ocamlbuild $(FLAGS) Reagents.cma

data: reagents
	ocamlbuild $(FLAGS) Reagents_data.cma

sync: reagents
	ocamlbuild $(FLAGS) Reagents_sync.cma

tests:
	ocamlbuild $(FLAGS) $(TEST)

clean:
	ocamlbuild -clean
	find . -name "*~" | xargs rm -f
