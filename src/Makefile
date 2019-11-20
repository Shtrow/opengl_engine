.SUFFIXES: .ml .mli .cmo .cmi

# L'éxécutable
EXEC = game
# Le compilateur 
CAMLC= ocamlc
# Les mli
ILM = *.mli
# Les ml
LM = *.ml

all:
	$(CAMLC) -o $(EXEC) $(ILM) $(LM)

clean:
	echo "Suppression du makefile"
	rm -f *.cm[io] *~ .*~ #*#
	rm -f $(EXEC)

