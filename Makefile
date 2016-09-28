KIND=native
TEST=test/swap_test.$(KIND) test/ref_test.$(KIND) test/counter_test.$(KIND) \
		 test/queue_test.$(KIND) test/stack_test.$(KIND) test/lock_test.$(KIND) \
		 test/dining_philosophers.$(KIND) test/rec_test.$(KIND) test/three_way.$(KIND) \
		 test/two_way.$(KIND) test/ref_channel.$(KIND) test/sat.$(KIND) \
		 test/swap_test2.$(KIND) test/stack_test_compose.$(KIND) test/tsx_test.$(KIND)
BENCH=test/reagent_queue.$(KIND) test/hw_queue.$(KIND) test/eli_stack.$(KIND) \
			test/trieber_stack.$(KIND)
FLAGS=-Is lib,data,sync -cflag -bin-annot -lib unix -cflag -g
BCFLAGS=-ocamlc ocamlcp -cflag -bin-annot -cflag -g -lflag -g $(FLAGS)
BAFLAGS=-cflag -annot -cflag -g -lflag -g $(FLAGS)
OCAMLBUILD=ocamlbuild

all: reagents data sync tests

reagents:
	$(OCAMLBUILD) $(FLAGS) Reagents.cma

data: reagents
	$(OCAMLBUILD) $(FLAGS) Reagents_data.cma

sync: reagents
	$(OCAMLBUILD) $(FLAGS) Reagents_sync.cma

tests:
	$(OCAMLBUILD) $(FLAGS) $(TEST)

bench-alloc:
	$(OCAMLBUILD) $(BAFLAGS) $(BENCH)

bench-count:
	$(OCAMLBUILD) $(BCFLAGS) $(BENCH)

clean:
	$(OCAMLBUILD) -clean
	rm -f *.preprof *.prof *.dump
	find . -name "*~" | xargs rm -f
