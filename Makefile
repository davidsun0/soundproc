FLAGS = -lpulse-simple -lm

all: soundproc

soundproc: soundproc.c
	$(CC) soundproc.c -o soundproc $(FLAGS)

clean:
	rm soundproc
