all: clean edit

edit:	asttest lextest typetest

asttest:
				cd ./src; dune build asttest.exe
lextest:
				cd ./src; dune build lextest.exe
typetest:
				cd ./src; dune build typetest.exe

clean:
				dune clean
