kiss: 
	gcc -Wall -O3 -o kiss *.c -L/usr/lib/ -lm -lgmp

debug: 
	gcc -Wall -g -O0 *.c -o kiss -lm -lgmp

profile: 
	gcc -Wall -O3 *.c -o kiss -L/usr/lib/ -lm -lgmp -pg 

$(objects) : kiss.h

.PHONY: clean
clean :
	rm -f kiss.exe kiss newfile example.dat



