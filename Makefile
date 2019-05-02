PACKS =
OCAMLYACC = menhir
SRCDIR = src
SOURCES = $(addprefix $(SRCDIR)/, \
					) \
					$(MAIN)
MAIN = $(SRCDIR)/main.ml
RESULT = prog

all: frontend

frontend: bc

include OCamlMakefile
