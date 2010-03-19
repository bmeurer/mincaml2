SOURCES=			\
	parsing/astcommon.mli	\
	parsing/lexer.ml	\
	parsing/lexer.mli	\
	parsing/location.ml	\
	parsing/location.mli	\
	parsing/parsedast.mli	\
	parsing/parser.ml	\
	parsing/parser.mli	\
	parsing/syntaxerr.ml	\
	parsing/syntaxerr.mli	\
	typing/ident.ml		\
	typing/ident.mli	\
	typing/predefined.ml	\
	typing/predefined.mli	\
	typing/typedast.mli	\
	typing/typeenv.mli	\
	typing/types.ml		\
	typing/types.mli	\
	utils/rbmap.ml		\
	utils/rbmap.mli		\
	utils/rbset.ml		\
	utils/rbset.mli

OBJECTS=$(patsubst %.ml,%.cmo,$(patsubst %.mli,%.cmi,$(SOURCES))) \
	$(patsubst %.ml,%.cmx,$(patsubst %.mli,%.cmi,$(SOURCES)))

OCAMLCOMMONFLAGS=-I parsing -I typing -I utils
OCAMLC=ocamlc.opt
OCAMLCFLAGS=$(OCAMLCOMMONFLAGS) -g
OCAMLDEP=ocamldep.opt
OCAMLDEPFLAGS=$(OCAMLCOMMONFLAGS)
OCAMLOPT=ocamlopt.opt
OCAMLOPTFLAGS=$(OCAMLCFLAGS)

all: $(OBJECTS)

.SUFFIXES: .ml .mli .mll .mly .cmi .cmo .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLCFLAGS) -c -o $@ $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c -o $@ $<

.mli.cmi:
	$(OCAMLC) $(OCAMLCFLAGS) -c -o $@ $<

.mll.ml:
	ocamllex -o $@ $<

%.ml %.mli: %.mly
	ocamlyacc $<

clean::
	rm -f $(OBJECTS)
	rm -f .depend
	rm -f parsing/lexer.ml parsing/parser.ml parsing/parser.mli

.depend: Makefile $(SOURCES)
	$(OCAMLDEP) $(OCAMLDEPFLAGS) $(SOURCES) > $@

include .depend

.PHONY: clean
