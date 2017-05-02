OCB_FLAGS = -use-ocamlfind
OCB = 		ocamlbuild $(OCB_FLAGS)

all: 		native byte # profile debug

clean:
			$(OCB) -clean

native:
			$(OCB) main.native

byte:
			$(OCB) main.byte

profile:
			$(OCB) -tag profile main.native

debug:
			$(OCB) -tag debug main.byte

test: 		native
			./main.native "OCaml" "OCamlBuild" "users"

.PHONY: 	all clean byte native profile debug test