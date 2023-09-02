.PHONY: all clean

MENHIR          := menhir

MENHIRFLAGS     := --infer

OCAMLBUILD      := ocamlbuild -use-ocamlfind -use-menhir -menhir "$(MENHIR) $(MENHIRFLAGS)"

MAIN            := oeval

all:
	$(OCAMLBUILD) $(MAIN).native
	
clean:
	rm -f *~ .*~
	rm *.cmi *.cmo
	$(OCAMLBUILD) -clean