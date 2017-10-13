passtable: passtable.o
	gcc passtable.o -Wall -llua5.3 -o passtable

callfunc: callfunc.o
	gcc callfunc.o -Wall -llua5.3 -o callfunc

hello: hello.o
	gcc hello.o -Wall -llua5.3 -o hello

.PHONY: clean

clean:
	rm -f *.o
