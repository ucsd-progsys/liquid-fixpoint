include config.make

DIRS=-I misc
OFLAGS=$(DIRS) $(IFLAGS) $(LFLAGS) $(CFLAGS)

LIBS_=-libs unix,str,graph
IFLAGS_=-lflags -I,$(OCAMLGRAPHHOME)
LFLAGS_=-lflags -cclib,-L$(OCAMLLIB) \

CFLAGS_=-cflags -dtypes,-annot \
		-cflags -I,$(OCAMLGRAPHHOME) \
		-cflags -thread

SMTZ3=smtZ3.ml

ifdef Z3HOME
  LIBS=$(LIBS_),z3
  IFLAGS=$(IFLAGS_) \
		 -lflags -I,$(Z3HOME)/lib \
		 -lflags -I,$(Z3HOME)/ocaml
  LFLAGS=$(LFLAGS_) \
		 -lflags -cc,g++ \
		 -lflags -cclib,-lstdc++ \
		 -lflags -cclib,-lcamlidl \
		 -lflags -cclib,-L$(Z3HOME)/lib \
		 -lflags -cclib,-lz3 \
		 -lflags -cclib,-lz3stubs \
		 -lflags -cclib,-fopenmp\
		 -lflags -cclib,-lrt
  CFLAGS=$(CFLAGS_) \
		 -cflags -I,$(Z3HOME)/ocaml
  SMTZ3SRC=smtZ3.mem.ml
else
  LIBS=$(LIBS_)
  IFLAGS=$(IFLAGS_)
  LFLAGS=$(LFLAGS_)
  CFLAGS=$(CFLAGS_)
  SMTZ3SRC=smtZ3.nomem.ml
endif

all: smtz3
	ln -sf ../misc
	ocamlbuild -r $(LIBS) $(OFLAGS) -tags thread fixpoint.native
	ocamlbuild -r $(OFLAGS) fix.cmxa
	rm -f fixpoint.native
	cp _build/fixpoint.native .

smtz3: $(SMTZ3SRC)
	cp $(SMTZ3SRC) $(SMTZ3)

clean:
	rm -rf *.byte *.native _build _log

fixtop:
	ocamlbuild -r $(LIBS) $(OFLAGS) fixtop.native

horn:
	ocamlbuild -r $(LIBS) $(OFLAGS) hornToInterproc.native

MLITARGET=toSmtLib.ml
mli:
	ocamlc -I _build/ -I _build/misc/ -I $(Z3HOME)/lib -I $(Z3HOME)/ocaml -i $(MLITARGET)


