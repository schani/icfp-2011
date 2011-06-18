#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
#include <sys/time.h>

#ifdef RAND_MAX
#undef RAND_MAX
#endif
#define RAND_MAX 256*15

void opp(void) {
	char buffer[255];
	int lr;
	char card[20];
	int slot;

	if (fgets(buffer, sizeof(buffer), stdin) != NULL) {
		lr = strtol(buffer, NULL, 10);
		if (lr == 1) {
			if (fgets(buffer, sizeof(buffer), stdin))
				strcpy(card, buffer);
			if (fgets(buffer, sizeof(buffer), stdin))
				slot = strtol(buffer, NULL, 10);
		}
		else {
			if (fgets(buffer, sizeof(buffer), stdin))
				slot = strtol(buffer, NULL, 10);
			if (fgets(buffer, sizeof(buffer), stdin))
				strcpy(card, buffer);
		}
	}
	//printf("oppp: lr=%d card=%s slot=%d\n", lr, card, slot);
}

int main(int argc, char *argv[]) {
	char cards[15][10] = {
		{"I"},
		{"zero"},
		{"succ"},
		{"dbl"},
		{"get"},
		{"put"},
		{"S"},
		{"K"},
		{"inc"},
		{"dec"},
		{"attack"},
		{"help"},
		{"copy"},
		{"revive"},
		{"zombie"}
	};

#ifndef SRAND_ZERO
	struct timeval tv;
	gettimeofday(&tv, NULL);
	srand ( tv.tv_sec*1000 + tv.tv_usec );
#else
	srand ( 0 );
#endif

	if (argc>1 && !strcmp(argv[1], "1"))
		opp();
	
	while (1) {
	
		int lr = rand()%2 + 1;
		int slot = rand()%256;
		int card = rand()%15;
		
		if (lr == 1)
			fprintf (stdout, "%d\n%s\n%d\n", lr, cards[card], slot);
		else
			fprintf (stdout, "%d\n%d\n%s\n", lr, slot, cards[card]);

		fflush(stdout);

		opp();
		
	}
	return 0;
}
