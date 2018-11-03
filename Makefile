all: clean edit

edit:	dlgcompile dlgvm copy

copy:
				mkdir bin;
				cp _build/default/src/dlgcompile.exe bin/dlgcompile
				cp _build/default/src/dlgvm.exe bin/dlgvm

dlgcompile:
				cd ./src; dune build dlgcompile.exe
dlgvm:
				cd ./src; dune build dlgvm.exe

clean:
				rm -rf bin/;
				dune clean
