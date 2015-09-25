TEST=test/swap_test.byte test/ref_test.byte test/counter_test.byte \
		 test/queue_test.byte test/stack_test.byte test/lock_test.byte
BENCH=test/reagent_queue.byte test/hw_queue.byte test/eli_stack.byte \
			test/trieber_stack.byte
FLAGS=-Is lib,data,sync -lib unix
BFLAGS=-cflag -annot -cflag -g -lflag -g $(FLAGS)

all: reagents data sync tests

reagents:
	ocamlbuild $(FLAGS) Reagents.cma

data: reagents
	ocamlbuild $(FLAGS) Reagents_data.cma

sync: reagents
	ocamlbuild $(FLAGS) Reagents_sync.cma

tests:
	ocamlbuild $(FLAGS) $(TEST)

bench:
	ocamlbuild $(BFLAGS) $(BENCH)

clean:
	ocamlbuild -clean
	rm -f *.preprof *.prof *.dump
	find . -name "*~" | xargs rm -f
