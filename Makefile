# FLAGS = 
EXEC = ../../bin/game
EXEC2 = bin/game
PKG = glMLite,glMLite.glut,tsdl,glMLite.vbo,glMLite.vertex_arrays,glfw-ocaml,glMLite.jpeg_loader,

all: ogl_matrix
	cd src/Render ; \
	ocamlfind ocamlopt -o $(EXEC) -linkpkg -package $(PKG) ogl_matrix.cmx render.ml ;\


gl: ogl_matrix
	cd src/Render ; \
	ocamlfind ocamlopt -o $(EXEC) -linkpkg -package $(PKG) ogl_matrix.cmx renderer.ml

test: ogl_matrix
	cd src/Render ; \
	ocamlfind ocamlopt -o $(EXEC) -linkpkg -package $(PKG) ogl_matrix.cmx test.ml

ogl_matrix: 
	cd src/Render ; \
	ocamlopt ogl_matrix.mli ; \
	ocamlopt ogl_matrix.ml