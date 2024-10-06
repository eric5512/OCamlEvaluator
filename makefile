.PHONY: all clean test

MENHIR          := menhir

MENHIRFLAGS     := --infer

OCAMLBUILD      := ocamlbuild -pkg unix -I src -use-ocamlfind -pkg oplot -use-menhir -menhir "$(MENHIR) $(MENHIRFLAGS)"

MAIN            := oeval

all:
	rm -f ./src/*.cmi
	$(OCAMLBUILD) $(MAIN).native
	cp ./_build/src/*.cmi ./src/
	mv $(MAIN).native $(MAIN)

test: all
	./test/test.sh

debug:
	rm -f ./src/*.cmi
	$(OCAMLBUILD) $(MAIN).d.byte
	cp ./_build/src/*.cmi ./src/
	mv $(MAIN).d.byte $(MAIN)

clean:
	rm -f *~ .*~
	$(OCAMLBUILD) -clean