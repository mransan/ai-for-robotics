#
# Pure OCaml, no packages, no _tags 
#

# bin-annot is required for Merlin and other IDE-like tools

OCB_FLAGS = -use-ocamlfind -pkg lacaml -pkg yojson -tag bin_annot -I src
OCB = 		ocamlbuild $(OCB_FLAGS)

all: 		native

clean:
			$(OCB) -clean

native: 
			$(OCB) src/lessons/lesson01.native
			$(OCB) src/lessons/lesson02.native
			$(OCB) src/lessons/lesson03.native
			$(OCB) src/tests/steering_robot_test.native
			$(OCB) src/tests/particle_filter_test.native
			$(OCB) src/tests/gaussian1D_test.native
			$(OCB) src/tests/velocity_model2D_test.native
			$(OCB) src/tests/eigen_values_test.native
			$(OCB) src/apps/pf2d.native
			$(OCB) src/apps/ekf1d.native
			$(OCB) src/apps/velocity_model_ekf.native
			$(OCB) src/apps/velocity_model_ekf2.native

.PHONY: 	all clean byte native 
