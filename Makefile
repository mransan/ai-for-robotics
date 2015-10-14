#
# Pure OCaml, no packages, no _tags 
#

# bin-annot is required for Merlin and other IDE-like tools

OCB_FLAGS = -use-ocamlfind -pkg lacaml -tag bin_annot -I src/
OCB = 		ocamlbuild $(OCB_FLAGS)

all: 		native

clean:
			$(OCB) -clean

native: 
			$(OCB) src/lesson01.native
			$(OCB) src/lesson02.native

.PHONY: 	all clean byte native 
