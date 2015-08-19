FLAGS=-Is lib -lib unix

all: reagents

reagents:
	ocamlbuild $(FLAGS) Reagents.cma

clean:
	ocamlbuild -clean
