kiss: 
	gcc -O3 -o kiss *.c -L/usr/lib/ -lm -lgmp

debug: 
	gcc -Wall -g -O0 *.c -o kiss -lm -lgmp

profile: 
	gcc -Wall -pg -O3 *.c -o kiss -L/usr/lib/ -lm -lgmp

$(objects) : kiss.h

.PHONY: clean
clean :
	rm -f kiss.exe kiss



