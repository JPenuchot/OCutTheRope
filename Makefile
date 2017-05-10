OCB_FLAGS = -use-ocamlfind -pkg cohttp.lwt
OCB = 		ocamlbuild $(OCB_FLAGS)

all: 		native # test # profile debug

clean:
			$(OCB) -clean

native:
			$(OCB) game.native
			$(OCB) editor.native
			$(OCB) download.native

byte:
			$(OCB) game.byte
			$(OCB) editor.byte
			$(OCB) download.byte

profile:
			$(OCB) -tag profile game.native
			$(OCB) -tag profile editor.native
			$(OCB) -tag profile download.native

debug:
			$(OCB) -tag debug game.byte
			$(OCB) -tag debug editor.byte
			$(OCB) -tag debug download.byte

test: 		
	$(OCB) -tag debug test.byte
	./test.byte

expe: 		
	$(OCB) -tag debug expe.byte

America\ great\ again:
	@echo We will build a wall! And Mexico will pay.

.PHONY: 	all clean byte native profile debug tests