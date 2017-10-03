objects = array.o cf_invoke.o character.o class.o control.o error.o environment.o eval.o feature.o \
          format.o function.o gc.o gf_invoke.o hash_table.o ilos.o init.o inline.o load.o \
          main.o number.o read.o repl.o sequence.o stream.o string.o symbols.o variable.o \
          vector.o wcs.o



kiss: $(objects)
	gcc -O3 -o kiss $(objects) -L/usr/lib/ -lm -lgmp

debug: 
	gcc -Wall -g -O0 *.c -o kiss -lm -lgmp

profile: 
	gcc -Wall -pg -O3 *.c -o kiss -L/usr/lib/ -lm -lgmp

$(objects) : kiss.h

.PHONY: clean
clean :
	rm kiss.exe kiss $(objects)


