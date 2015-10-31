#
# Pure OCaml, no packages, no _tags 
#

# bin-annot is required for Merlin and other IDE-like tools

OCB_FLAGS = -use-ocamlfind -pkg yojson -tag bin_annot -I src/
OCB = 		ocamlbuild $(OCB_FLAGS)

all: 		native

clean:
			$(OCB) -clean

native: 
			$(OCB) src/lessons/lesson01.native
			#$(OCB) src/lessons/lesson02.native
			$(OCB) src/lessons/lesson03.native
			$(OCB) src/tests/steering_robot_test.native
			$(OCB) src/tests/particle_filter_test.native
			$(OCB) src/tests/gaussian1D_test.native
			$(OCB) src/apps/pf2d.native

.PHONY: 	all clean byte native 
