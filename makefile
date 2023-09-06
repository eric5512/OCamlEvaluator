.PHONY: all clean test

MENHIR          := menhir

MENHIRFLAGS     := --infer

OCAMLBUILD      := ocamlbuild -I src -use-ocamlfind -use-menhir -menhir "$(MENHIR) $(MENHIRFLAGS)"

MAIN            := oeval

all:
	$(OCAMLBUILD) $(MAIN).native

test: all
	./test/test.sh

clean:
	rm -f *~ .*~
	rm *.cmi *.cmo
	$(OCAMLBUILD) -clean