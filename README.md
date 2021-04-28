## Game by Benjamin Viau

# Compilation

## Dependencies 
* ocaml >= 4.08.1
* dune
* glMLite
* glfw-ocaml
* sfml

If you have opam, type
`opam install . --deps-only`
to install dependencies

`make`
to compile, and
`./game.exe`
to run

# How to play the game

* `Arrow keys` to change direction
* `Space` to move
* `W` to wait
* `S` to shoot
* `R` to retry level

# Known issues

* Nowadays, glMLite opam package doesn't automatically compile the PNG module.
  You can run `fix_png.sh` to fix that.
