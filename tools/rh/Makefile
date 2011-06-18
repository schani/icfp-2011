all: rh rh0

rh : rh.c
	$(CC) -Wall -O2 -o $@ $^

rh0 : rh.c
	$(CC) -Wall -O2 -DSRAND_ZERO -o $@ $^

clean:
	$(RM) rh rh0
	
