all: main.exe

main.exe: main.ml
	ocamlfind ocamlopt -o main.exe \
		-linkpkg -package stdio \
		-linkpkg -package base \
		main.ml

clean:
	rm -f *.exe *.o *.cmx *.cmi
