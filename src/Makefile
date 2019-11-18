.SUFFIXES: .ml .mli .cmo .cmi

# L'éxécutable
EXEC = game
# Le compilateur 
CAMLC= ocamlc
FLAGS= -g -I
LIBS = +glMLite


all:
	$(CAMLC) engine.mli
	$(CAMLC) engine.ml
	$(CAMLC) -o $(EXEC) $(FLAGS) $(LIBS) engine.cmo prototype.ml

clean:
	echo "Suppression du makefile"
	rm -f *.cm[io] *~ .*~ #*#
	rm -f $(EXEC)

