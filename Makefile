SOURCES=			\
	parsing/astcommon.mli	\
	parsing/location.ml	\
	parsing/location.mli	\
	parsing/parsedast.mli	\
	parsing/syntaxerr.ml	\
	parsing/syntaxerr.mli	\
	parsing/parser.ml	\
	parsing/parser.mli	\
	parsing/lexer.ml	\
	parsing/lexer.mli	\
	parsing/parse.ml	\
	parsing/parse.mli	\
	typing/ident.ml		\
	typing/ident.mli	\
	typing/primitive.ml	\
	typing/primitive.mli	\
	typing/types.ml		\
	typing/types.mli	\
	typing/typedast.ml	\
	typing/typedast.mli	\
	typing/typeenv.ml	\
	typing/typeenv.mli	\
	typing/printtyp.ml	\
	typing/printtyp.mli	\
	typing/typing.ml	\
	typing/typing.mli	\
	toplevel.ml

OBJECTS=$(patsubst %.ml,%.cmo,$(patsubst %.mli,%.cmi,$(SOURCES))) \
	$(patsubst %.ml,%.cmx,$(patsubst %.mli,%.cmi,$(SOURCES)))

OCAMLCOMMONFLAGS=-I parsing -I typing
OCAMLC=ocamlc.opt
OCAMLCFLAGS=$(OCAMLCOMMONFLAGS) -g
OCAMLDEP=ocamldep.opt
OCAMLDEPFLAGS=$(OCAMLCOMMONFLAGS)
OCAMLOPT=ocamlopt.opt
OCAMLOPTFLAGS=$(OCAMLCFLAGS)

all: toplevel

toplevel: $(filter %.cmo,$(OBJECTS))
	$(OCAMLC) $(OCAMLOPTFLAGS) -o $@ $^

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
	rm -f toplevel

.depend: Makefile $(SOURCES)
	$(OCAMLDEP) $(OCAMLDEPFLAGS) $(SOURCES) > $@

include .depend

.PHONY: clean
