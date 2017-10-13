CFLAGS = -Wall -llua5.3

%.o: %.c
	gcc -c -o $@ $< $(CFLAGS)

f: f.o
	gcc -o $@ $< $(CFLAGS)

generic: generic.o
	gcc -o $@ $< $(CFLAGS)


generic_bool: generic_bool.o
	gcc -o $@ $< $(CFLAGS)

.PHONY: clean

clean:
	rm -f *.o *~
