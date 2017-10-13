CFLAGS = -Wall -llua5.3

%.o: %.c
	gcc -c -o $@ $< $(CFLAGS)

manip_01: manip_01.o
	gcc -o $@ $< $(CFLAGS)

manip_02: manip_02.o
	gcc -o $@ $< $(CFLAGS)

.PHONY: clean

clean:
	rm -f *.o *~
