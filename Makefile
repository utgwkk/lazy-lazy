PACKS =
OCAMLYACC = menhir
SRCDIR = src
SOURCES = $(addprefix $(SRCDIR)/, \
					syntax.ml \
					frontend/parser.mly \
					frontend/lexer.mll \
					eval.ml \
					lazyEval.ml \
					) \
					$(MAIN)
MAIN = $(SRCDIR)/main.ml
RESULT = prog

all: frontend

frontend: bc

include OCamlMakefile
