
all:	tc

tc :	tc.c
	$(CC) -Wall -std=c99 -O9 -o $@ $^

tc.debug :	tc.c
	$(CC) -Wall -std=c99 -O2 -g -o $@ $^

clean:
	$(RM) tc tc.debug
	
