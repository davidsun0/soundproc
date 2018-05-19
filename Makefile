CFLAGS = -lpulse-simple -lm

all:
	soundproc

soundproc:
	$(CC) -o soundproc soundproc.c $(CFLAGS)

