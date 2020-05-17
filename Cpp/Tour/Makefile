CC=g++
CFLAGS=-pipe -std=c++14 -Wall

.PHONY: Vector_test, out_dir, clean

vector_test: build_vector_test
	out\Vector_test.exe

build_vector_test: make_out_dir
	$(CC) $(CFLAGS) Vector.cpp Vector_test.cpp -o out\Vector_test.exe

make_out_dir:
	mkdir -p out

clean:
	rm -r out