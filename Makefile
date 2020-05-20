DUNE_FLAGS = --profile release
EXEC = game.exe
all:
	cd src ;\
	dune build $(EXEC) $(DUNE_FLAGS) ;\
	cd ../ ;\
	ln -sf _build/default/src/$(EXEC) ./$(EXEC)
