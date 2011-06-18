#include <stdio.h>
#include <string.h>
#include <stdlib.h>

struct cmd {
	int lr;
	char card[10];
	int slot;
};


void opp(struct cmd *oppcmd) {
	char buffer[255];
	int lr = 0;
	char card[20];
	int slot = 0;

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
	char *end = strchr(card, '\n');
	*end = 0;
	
	//printf("oppp: lr=%d card=%s slot=%d\n", lr, card, slot);
	
	oppcmd->lr = lr;
	strcpy(oppcmd->card, card);
	oppcmd->slot = slot;
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

	struct cmd lastopp_cmd;
	lastopp_cmd.lr = 1;
	strcpy(lastopp_cmd.card, cards[0]);
	lastopp_cmd.slot = 0;
		
	if (argc>1 && !strcmp(argv[1], "1"))
		opp(&lastopp_cmd);
	
	while (1) {
		if (lastopp_cmd.lr == 1)
			fprintf (stdout, "%d\n%s\n%d\n", lastopp_cmd.lr, lastopp_cmd.card, lastopp_cmd.slot);
		else
			fprintf (stdout, "%d\n%d\n%s\n", lastopp_cmd.lr, lastopp_cmd.slot, lastopp_cmd.card);

		fflush(stdout);

		opp(&lastopp_cmd);
		
	}
	return 0;
}
