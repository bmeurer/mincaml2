SOURCES=				\
	utils/hashtblUtils.ml		\
	utils/hashtblUtils.mli		\
	utils/listUtils.ml		\
	utils/listUtils.mli		\
	parsing/astcommon.mli		\
	parsing/location.ml		\
	parsing/location.mli		\
	parsing/parsedast.mli		\
	parsing/syntaxerr.ml		\
	parsing/syntaxerr.mli		\
	parsing/parser.ml		\
	parsing/parser.mli		\
	parsing/lexer.ml		\
	parsing/lexer.mli		\
	parsing/parse.ml		\
	parsing/parse.mli		\
	typing/ident.ml			\
	typing/ident.mli		\
	typing/primitive.ml		\
	typing/primitive.mli		\
	typing/types.ml			\
	typing/types.mli		\
	typing/typedast.ml		\
	typing/typedast.mli		\
	typing/typeenv.ml		\
	typing/typeenv.mli		\
	typing/printtyp.ml		\
	typing/printtyp.mli		\
	typing/typing.ml		\
	typing/typing.mli		\
	lambda/lambda.ml		\
	lambda/lambda.mli		\
	lambda/printlambda.ml		\
	lambda/printlambda.mli		\
	lambda/translpat.ml		\
	lambda/translpat.mli		\
	lambda/translexp.ml		\
	lambda/translexp.mli		\
	lambda/closure.ml		\
	lambda/closure.mli		\
	codegen/codegen.ml		\
	codegen/codegen.mli		\
	toplevel.ml

OBJECTS=$(patsubst %.ml,%.cmo,$(patsubst %.mli,%.cmi,$(SOURCES))) \
	$(patsubst %.ml,%.cmx,$(patsubst %.mli,%.cmi,$(SOURCES)))

OCAMLCOMMONFLAGS=-I codegen -I lambda -I parsing -I typing -I utils
OCAMLC=ocamlc.opt
OCAMLCFLAGS=$(OCAMLCOMMONFLAGS) -g -warn-error A
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
	ocamlyacc -v $<

clean::
	rm -f $(OBJECTS)
	rm -f .depend
	rm -f parsing/lexer.ml
	rm -f parsing/parser.ml parsing/parser.mli parsing/parser.output
	rm -f toplevel

.depend: Makefile $(SOURCES)
	$(OCAMLDEP) $(OCAMLDEPFLAGS) $(SOURCES) > $@

include .depend

.PHONY: clean
