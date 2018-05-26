FLAGS = -g -Wall -lpulse-simple -lm
CC = g++

all: composition

composition: soundproc.o composition.o
	$(CC) soundproc.o composition.o -o composition $(FLAGS)

composition.o: composition.c
	$(CC) composition.c -c $(FLAGS)

soundproc.o: soundproc.c soundproc.h
	$(CC) soundproc.c -c $(FLAGS)

clean:
	rm -f composition *.o
