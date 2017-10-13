CFLAGS = -Wall -llua5.3

%.o: %.c
	gcc -c -o $@ $< $(CFLAGS)

dir: dir.o
	gcc -o $@ $< $(CFLAGS)

sin: sin.o
	gcc -o $@ $< $(CFLAGS)

.PHONY: clean

clean:
	rm -f *.o *~
