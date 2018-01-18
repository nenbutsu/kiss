objects = array.o cf_invoke.o character.o class.o control.o environment.o error.o \
          feature.o format.o function.o gc.o gf_invoke.o hash_table.o ilos.o init.o \
          inline.o invoke.o main.o misc.o number.o read.o repl.o sequence.o stream.o \
          string.o symbol.o variable.o vector.o wcs.o

CFLAGS= -Wall -O3

all: $(objects)
	cc -Wall -o kiss $(objects) -L/usr/lib/ -lm -lgmp

.PHONY: debug
debug: 
	make "CFLAGS= -Wall -g -O0"

.PHONY: profile
profile: 
	make "CFLAGS= -Wall -pg -O3"

$(objects) : kiss.h

.PHONY: clean
clean :
	rm -f kiss.exe kiss newfile example.dat *.o
