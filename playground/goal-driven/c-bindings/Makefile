all:
	pintc -output-obj -o pint_gdr_caml.o pint_gdr_c.ml
	gcc -c pint_gdr_wrap.c
	gcc main.c pint_gdr_wrap.o pint_gdr_caml.o -L/usr/lib/ocaml -lunix -lasmrun -lm -ldl -lnums ../../../libpint_stubs.a -lcamlstr -lRmath

