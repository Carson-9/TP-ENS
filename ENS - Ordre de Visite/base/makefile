TARGET	= main
CC	= gcc
CFLAGS	= -Wall -std=c99 -g
SOURCE	= main.c graphe.c utils.c
OBJECTS	= main.o graphe.o utils.o
INCLUDES= graphe.h utils.h

$(TARGET): $(OBJECTS)
	$(CC) -o $@ $^ $(CFLAGS)

%.o: %.c $(INCLUDES) 
	$(CC) -o $@ -c $< $(CFLAGS)

clean:
	rm *.o main

tar:
	tar -zcvf base.tar.gz graphes.txt makefile $(SOURCE) $(INCLUDES)
