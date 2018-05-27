FLAGS = -g -Wall -lm -lpulse-simple
CC = g++

all: composition

composition: soundproc.o composition.o instruments.o
	$(CC) soundproc.o composition.o instruments.o -o composition $(FLAGS)

composition.o: composition.c
	$(CC) composition.c -c $(FLAGS)

instruments.o: instruments.c instruments.h
	$(CC) instruments.c -c $(FLAGS)

soundproc.o: soundproc.c soundproc.h
	$(CC) soundproc.c -c $(FLAGS)

clean:
	rm -f composition *.o
