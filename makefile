all: /*
	ocamlc -g unix.cma graphics.cma colors.mli colors.ml main.ml -o main

clean:
	@rm *.cmi *.cmo
	@echo -e "\e[92mCompilation finished!\e[0m"