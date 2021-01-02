CFLAGS = -g -Wall

ccombine: ccombine.o
	$(CC) $(CFLAGS) -o ccombine ccombine.o -lm
