LEX = ocamllex
YACC = menhir
CC = ocamlfind ocamlopt
CCFLAGS = -linkpkg -package sexplib,ppx_sexp_conv,ANSITerminal

all: clean edit

edit:	build/dlgAST.cmx build/position.cmx build/error.cmx build/dlgLexer.cmx build/dlgParser.cmx
				$(CC) -o build/dlglex.native $(CCFLAGS) -I build/ build/position.cmx build/error.cmx build/dlgLexer.cmx test/lextest.ml
				$(CC) -o build/dlgast.native $(CCFLAGS) -I build/ build/position.cmx build/error.cmx build/dlgAST.cmx build/dlgParser.cmx build/dlgLexer.cmx test/asttest.ml
				mv test/*.cmi test/*.cmx test/*.o build/
				mv build/dlglex.native bin/dlglex
				mv build/dlgast.native bin/dlgast

build/position.cmx:
				$(CC) -c $(CCFLAGS) lib/position.ml -o build/position.cmx
build/error.cmx: build/position.cmx
				$(CC) -c $(CCFLAGS) -linkpkg -package ANSITerminal -I build/ lib/error.ml -o build/error.cmx
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
	    rm -rf test/*.cmi test/*.cmx test/*.o test/*.cmt
	    rm -rf lib/*.cmi lib/*.cmx lib/*.o lib/*.cmt
	    rm -rf grammar/*.cmi grammar/*.cmx grammar/*.o grammar/*.cmt
			rm -rf build/*.native build/*.ml build/*.mli build/*.cmi build/*.cmx build/*.o build/*.cmt
	    echo Successfully cleaned project
