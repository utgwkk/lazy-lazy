PACKS =
OCAMLYACC = menhir
SRCDIR = src
SOURCES = $(addprefix $(SRCDIR)/, \
					syntax.ml \
					frontend/parser.mly \
					frontend/lexer.mll \
					) \
					$(MAIN)
MAIN = $(SRCDIR)/main.ml
RESULT = prog

all: frontend

frontend: bc

include OCamlMakefile
