LEX = ocamllex
YACC = menhir
CC = ocamlfind ocamlopt
CCFLAGS = -linkpkg -package sexplib,ppx_sexp_conv,ANSITerminal

all: clean edit

edit:	build/dlgAST.cmx build/position.cmx build/error.cmx build/dlgLexer.cmx build/dlgParser.cmx
				$(CC) -o build/dlglex.native $(CCFLAGS) -I build/ build/position.cmx build/error.cmx build/dlgLexer.cmx tests/lextest.ml
				$(CC) -o build/dlgast.native $(CCFLAGS) -I build/ build/position.cmx build/error.cmx build/dlgAST.cmx build/dlgParser.cmx build/dlgLexer.cmx tests/asttest.ml
				mv tests/*.cmi tests/*.cmx tests/*.o build/
				mv build/dlglex.native bin/dlglex
				mv build/dlgast.native bin/dlgast

build/position.cmx:
				$(CC) -c $(CCFLAGS) libs/position.ml -o build/position.cmx
build/error.cmx: build/position.cmx
				$(CC) -c $(CCFLAGS) -linkpkg -package ANSITerminal -I build/ libs/error.ml -o build/error.cmx
build/dlgAST.cmx: build/position.cmx
				$(CC) -c $(CCFLAGS) -I build/ grammar/dlgAST.ml -o build/dlgAST.cmx
build/dlgLexer.cmx: build/dlgLexer.ml build/dlgParser.cmx
				$(CC) -c $(CCFLAGS) build/dlgLexer.ml -I build/ -o build/dlgLexer.cmx

build/dlgParser.cmx: build/dlgParser.ml
				$(CC) -c $(CCFLAGS) build/dlgParser.mli -I build/ -o build/dlgParser.cmi
				$(CC) -c $(CCFLAGS) build/dlgParser.ml -I build/ -o build/dlgParser.cmx

build/dlgLexer.ml:
				$(LEX) grammar/dlg.mll -o build/dlgLexer.ml
build/dlgParser.ml:
				$(YACC) grammar/dlg.mly --explain --base build/dlgParser --fixed-exception

clean:
	    rm -rf tests/*.cmi tests/*.cmx tests/*.o tests/*.cmt
	    rm -rf libs/*.cmi libs/*.cmx libs/*.o libs/*.cmt
	    rm -rf grammar/*.cmi grammar/*.cmx grammar/*.o grammar/*.cmt
			rm -rf build/*.native build/*.ml build/*.mli build/*.cmi build/*.cmx build/*.o build/*.cmt
	    echo Successfully cleaned project
