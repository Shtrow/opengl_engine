.SUFFIXES: .ml .mli .cmo .cmi

# L'éxécutable
EXEC = game
# Le compilateur 
CAMLC= ocamlc

all:
	$(CAMLC) engine.mli
	$(CAMLC) engine.ml
	$(CAMLC) -o $(EXEC) engine.cmo test.ml

clean:
	rm -f *.cm[io] *~ .*~ #*#
	rm -f $(EXEC)

