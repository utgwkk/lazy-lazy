.PHONY: all clean byte native

OCB_FLAGS = -use-ocamlfind -I src -I src/frontend
OCB = ocamlbuild $(OCB_FLAGS)

all: frontend

frontend: native

native: sanity
	$(OCB) main.native

byte: sanity
	$(OCB) main.byte

clean:
	$(OCB) -clean

sanity:
	ocamlfind query benchmark
