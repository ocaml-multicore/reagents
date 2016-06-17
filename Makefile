TEST=test/swap_test.native test/ref_test.native test/counter_test.native \
		 test/queue_test.native test/stack_test.native test/lock_test.native \
		 test/dining_philosophers.native \
		 test/rec_test.native
BENCH=test/reagent_queue.native test/hw_queue.native test/eli_stack.native \
			test/trieber_stack.native
FLAGS=-Is lib,data,sync -cflag -annot -lib unix
BCFLAGS=-ocamlc ocamlcp -cflag -annot -cflag -g -lflag -g $(FLAGS)
BAFLAGS=-cflag -annot -cflag -g -lflag -g $(FLAGS)

all: reagents data sync tests

reagents:
	ocamlbuild $(FLAGS) Reagents.cma

data: reagents
	ocamlbuild $(FLAGS) Reagents_data.cma

sync: reagents
	ocamlbuild $(FLAGS) Reagents_sync.cma

tests:
	ocamlbuild $(FLAGS) $(TEST)

bench-alloc:
	ocamlbuild $(BAFLAGS) $(BENCH)

bench-count:
	ocamlbuild $(BCFLAGS) $(BENCH)

clean:
	ocamlbuild -clean
	rm -f *.preprof *.prof *.dump
	find . -name "*~" | xargs rm -f
