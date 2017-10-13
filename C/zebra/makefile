CC = gcc
CFLAGS = -Wall -I.
LIBS =
LIBS_THREADS = -pthread
DEPS = defs.h
ODIR = obj
OFILES = main.o perms.o
OBJ = $(patsubst %,$(ODIR)/%,$(OFILES))
OFILES_THREADS = main_threads.o perms.o
OBJ_THREADS = $(patsubst %,$(ODIR)/%,$(OFILES_THREADS))

zebra: $(OBJ)
	$(CC) $(CFLAGS) -o $@ $^ $(LIBS)

zebra_threads: $(OBJ_THREADS)
	$(CC) $(CFLAGS) -o $@ $^ $(LIBS_THREADS)

$(ODIR)/%.o: %.c $(DEPS)
	@mkdir -p $(ODIR)
	$(CC) $(CFLAGS) -c -o $@ $<


.PHONY: clean

clean:
	rm -rf $(ODIR)
	rm -f zebra
	rm -f zebra_threads