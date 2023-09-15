.PHONY: all clean test

MENHIR          := menhir

MENHIRFLAGS     := --infer

OCAMLBUILD      := ocamlbuild -I src -use-ocamlfind -use-menhir -menhir "$(MENHIR) $(MENHIRFLAGS)"

MAIN            := oeval

all:
	rm -f ./src/*.cmi
	$(OCAMLBUILD) $(MAIN).native
	cp ./_build/src/*.cmi ./src/

test: all
	./test/test.sh

clean:
	rm -f *~ .*~
	$(OCAMLBUILD) -clean