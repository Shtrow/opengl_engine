.SUFFIXES: .ml .mli .cmo .cmi

# L'éxécutable
EXEC = game
# Le compilateur 
CAMLC= ocamlc
FLAGS= -g

all:
	$(CAMLC) engine.mli
	$(CAMLC) engine.ml
	$(CAMLC) -o $(EXEC) $(FLAGS) engine.cmo prototype.ml

clean:
	rm -f *.cm[io] *~ .*~ #*#
	rm -f $(EXEC)

