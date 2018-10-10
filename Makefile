LEX = ocamllex
YACC = menhir
CC = ocamlfind ocamlopt
CCFLAGS = -linkpkg -package sexplib,ppx_sexp_conv

all: clean edit

edit:	bin/dlgAST.cmx bin/position.cmx bin/error.cmx bin/lexer.cmx bin/parser.cmx
				$(CC) -o bin/dlg.native -linkpkg -package sexplib,ppx_sexp_conv -I bin/ bin/position.cmx bin/error.cmx bin/lexer.cmx libs/main.ml

bin/position.cmx:
				$(CC) -c $(CCFLAGS) libs/position.ml -o bin/position.cmx
bin/error.cmx: bin/position.cmx
				$(CC) -c $(CCFLAGS) -I bin/ libs/error.ml -o bin/error.cmx
bin/dlgAST.cmx: bin/position.cmx
				$(CC) -c $(CCFLAGS) -I bin/ grammar/dlgAST.ml -o bin/dlgAST.cmx
bin/lexer.cmx: bin/lexer.ml bin/parser.cmx
				$(CC) -c $(CCFLAGS) bin/lexer.ml -I bin/ -o bin/lexer.cmx

bin/parser.cmx: bin/parser.ml
				$(CC) -c $(CCFLAGS) bin/parser.mli -I bin/ -o bin/parser.cmi
				$(CC) -c $(CCFLAGS) bin/parser.ml -I bin/ -o bin/parser.cmx


bin/lexer.ml:
				$(LEX) grammar/dlg.mll -o bin/lexer.ml
bin/parser.ml:
				$(YACC) grammar/dlg.mly --explain --base bin/parser

clean:
	    rm -rf libs/*.cmi libs/*.cmx libs/*.o
	    rm -rf grammar/*.cmi grammar/*.cmx grammar/*.o
			rm -rf bin/*.native bin/*.ml bin/*.mli bin/*.cmi bin/*.cmx bin/*.o
	    echo Successfully cleaned project
