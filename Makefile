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
	typing/identSet.ml		\
	typing/identSet.mli		\
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

RTSOURCES=				\
	runtime/mc2core.c		\
	runtime/mc2core.h		\
	runtime/mc2custom.c		\
	runtime/mc2custom.h		\
	runtime/mc2eh.c			\
	runtime/mc2eh.h			\
	runtime/mc2io.c			\
	runtime/mc2io.h			\
	runtime/mc2string.c		\
	runtime/mc2string.h		\
	runtime/mc2types.h

OBJECTS=$(patsubst %.ml,%.cmo,$(patsubst %.mli,%.cmi,$(SOURCES))) \
	$(patsubst %.ml,%.cmx,$(patsubst %.mli,%.cmi,$(SOURCES)))

RTOBJECTS=$(patsubst %.c,%.o,$(filter %.c,$(RTSOURCES)))

CC=gcc
CPPFLAGS=-I runtime -I /opt/local/include
CFLAGS=$(CPPFLAGS) -O2 -Wall -Werror
LIBTOOL=libtool

OCAMLCOMMONFLAGS=-I codegen -I lambda -I parsing -I typing -I utils
OCAMLC=ocamlc.opt
OCAMLCFLAGS=$(OCAMLCOMMONFLAGS) -g -warn-error A
OCAMLDEP=ocamldep.opt
OCAMLDEPFLAGS=$(OCAMLCOMMONFLAGS)
OCAMLOPT=ocamlopt.opt
OCAMLOPTFLAGS=$(OCAMLCFLAGS)
OCAMLLINKFLAGS=$(OCAMLOPTFLAGS) -cclib -L/opt/local/lib -cclib -lstdc++

all: toplevel runtime/mc2rt.a

toplevel: $(filter %.cmo,$(OBJECTS))
	$(OCAMLC) $(OCAMLLINKFLAGS) llvm.cma llvm_analysis.cma llvm_bitwriter.cma llvm_scalar_opts.cma -o $@ $^

runtime/mc2rt.a: $(RTOBJECTS)
	$(LIBTOOL) -static -o $@ $^

.SUFFIXES: .c .ml .mli .mll .mly .cmi .cmo .cmx

.c.o:
	$(CC) $(CFLAGS) -c -o $@ $<

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
	rm -f $(OBJECTS) $(RTOBJECTS)
	rm -f .depend
	rm -f parsing/lexer.ml
	rm -f parsing/parser.ml parsing/parser.mli parsing/parser.output
	rm -f toplevel

.depend: Makefile $(SOURCES) $(RTSOURCES)
	$(OCAMLDEP) $(OCAMLDEPFLAGS) $(SOURCES) > $@
	makedepend $(CPPFLAGS) -a -f - -- $(RTSOURCES) >> $@

include .depend

.PHONY: clean
