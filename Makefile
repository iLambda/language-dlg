LEX = ocamllex
YACC = menhir
CC = ocamlfind ocamlopt
CCFLAGS = -linkpkg -package sexplib,ppx_sexp_conv

all: clean edit

edit:	build/dlgAST.cmx build/position.cmx build/error.cmx build/lexer.cmx build/parser.cmx
				$(CC) -o build/dlglex.native -linkpkg -package sexplib,ppx_sexp_conv -I build/ build/position.cmx build/error.cmx build/lexer.cmx tests/lextest.ml
				mv tests/*.cmi tests/*.cmt tests/*.cmx tests/*.o build/
				mv build/dlglex.native bin/dlglex

build/position.cmx:
				$(CC) -c $(CCFLAGS) libs/position.ml -o build/position.cmx
build/error.cmx: build/position.cmx
				$(CC) -c $(CCFLAGS) -I build/ libs/error.ml -o build/error.cmx
build/dlgAST.cmx: build/position.cmx
				$(CC) -c $(CCFLAGS) -I build/ grammar/dlgAST.ml -o build/dlgAST.cmx
build/lexer.cmx: build/lexer.ml build/parser.cmx
				$(CC) -c $(CCFLAGS) build/lexer.ml -I build/ -o build/lexer.cmx

build/parser.cmx: build/parser.ml
				$(CC) -c $(CCFLAGS) build/parser.mli -I build/ -o build/parser.cmi
				$(CC) -c $(CCFLAGS) build/parser.ml -I build/ -o build/parser.cmx


build/lexer.ml:
				$(LEX) grammar/dlg.mll -o build/lexer.ml
build/parser.ml:
				$(YACC) grammar/dlg.mly --explain --base build/parser

clean:
	    rm -rf tests/*.cmi tests/*.cmx tests/*.o tests/*.cmt
	    rm -rf libs/*.cmi libs/*.cmx libs/*.o libs/*.cmt
	    rm -rf grammar/*.cmi grammar/*.cmx grammar/*.o grammar/*.cmt
			rm -rf build/*.native build/*.ml build/*.mli build/*.cmi build/*.cmx build/*.o build/*.cmt
	    echo Successfully cleaned project
