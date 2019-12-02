# FLAGS = 
EXEC = bin/game
PKG = glMLite,glMLite.glut,tsdl,glMLite.vbo,glMLite.vertex_arrays,glfw-ocaml,glMLite.jpeg_loader

all:
	ocamlfind ocamlopt -o $(EXEC) -linkpkg -package $(PKG) src/render.ml