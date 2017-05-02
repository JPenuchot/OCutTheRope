OCB_FLAGS = -use-ocamlfind
OCB = 		ocamlbuild $(OCB_FLAGS)

all: 		native # test # profile debug

clean:
			$(OCB) -clean

native:
			$(OCB) game.native
			$(OCB) editor.native

byte:
			$(OCB) game.byte
			$(OCB) editor.byte

profile:
			$(OCB) -tag profile game.native
			$(OCB) -tag profile editor.native

debug:
			$(OCB) -tag debug game.byte
			$(OCB) -tag debug editor.byte

test: 		
	$(OCB) -tag debug test.byte
	./test.byte

expe: 		
	$(OCB) -tag debug expe.byte

.PHONY: 	all clean byte native profile debug tests