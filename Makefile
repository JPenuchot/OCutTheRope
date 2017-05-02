OCB_FLAGS = -use-ocamlfind
OCB = 		ocamlbuild $(OCB_FLAGS)

all: 		native byte debug # profile debug

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

test: 		
	$(OCB) -tag debug test.byte
	./test.byte

expe: 		
	$(OCB) -tag debug expe.byte

.PHONY: 	all clean byte native profile debug tests