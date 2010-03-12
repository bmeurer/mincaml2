SOURCES=		\
	listutils.ml	\
	syntax.ml	\
	type.ml		\
	typing.ml

OBJECTS=$(patsubst %.ml,%.cmo,$(patsubst %.mli,%.cmi,$(SOURCES)))

all: $(OBJECTS)

.SUFFIXES: .ml .mli .mll .mly .cmi .cmo .cmx

.ml.cmo:
	ocamlc.opt -c -o $@ $<

.mli.cmi:
	ocamlc.opt -c -o $@ $<

.mll.ml:
	ocamllex -o $@ $<

%.ml %.mli: %.mly
	ocamlyacc $<

clean::
	rm -f *.cmi
	rm -f *.cmo
	rm -f .depend

.depend: Makefile $(SOURCES)
	ocamldep.opt $(SOURCES) > $@

include .depend

.PHONY: clean
