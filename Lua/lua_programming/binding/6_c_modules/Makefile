summation: summation.c
	gcc -shared -Wall -llua5.3 -o summation.so -fPIC summation.c

.PHONY: clean

clean:
	rm -f *.o *~
