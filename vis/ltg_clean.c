
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <getopt.h>
#include <alloca.h>

#include <stdint.h>
#include <stdbool.h>
#include <math.h>



uint64_t turn = 0;
int play_id = 0;
int card_id = 0;
int slot_id = 0;
int vitality = 0;


void	parse_turn_info(char *line)
{
	sscanf(line, "###### turn %ld", &turn);
	printf("\nturn %ld\n", turn);  
}

void	parse_player_info(char *line)
{
	char card_name[64];
	int n;

	n = sscanf(line, "player %d applied card %s to slot %d",
		&play_id, card_name, &slot_id);
	if (n == 3) {
		printf("%c: %s -> [%d]\n", 
			play_id ? 'B' : 'A',
			card_name, slot_id);  
		return;
	}

	n = sscanf(line, "player %d applied slot %d to card %s",
		&play_id, &slot_id, card_name);
	if (n == 3) {
		printf("%c: [%d] -> %s\n",
			play_id ? 'B' : 'A',
			slot_id, card_name);  
		return;
	}
}

void	parse_player_turn(char *line)
{
	sscanf(line, "*** player %d'", &play_id);
	printf("player %c\n", play_id ? 'B' : 'A');  
}

void	parse_slot(char *line)
{
	static char expr[65535];
	int n = sscanf(line, "%d={%d,%[^}]", &slot_id, &vitality, expr);
	if (n == 3) {
		printf("[%d]: |%d| %s\n", slot_id, vitality, expr);  
	}
	return;	
}

void	parse_expr(char *line)
{
	/* (slots ... ) and (1) ignored for now */
	return;	
}

void	parse_exception(char *line)
{
	printf("%s\n", line);
	return;	
}

enum	_state {
	ST_INIT	= 0,
	ST_SCAN	= 1,
	ST_TURN = 2,
};


void	parse_input(char *line)
{
	static char state = ST_INIT;
	
	switch (line[0]) {
	case 'L':	/* Lambda */
		state = ST_SCAN;
	
	case '#':	/* turn info */
		parse_turn_info(line);
		break;
		
	case 'p':	/* player info */
		parse_player_info(line);
		break;

	case '*':	/* players turn */
		parse_player_turn(line);
		state = ST_TURN;
		break;

	case '(':	/* refine */
		parse_expr(line);
		state = ST_SCAN;
		break;

	case 'E':	/* exception */
		parse_exception(line);
		break;

	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
		parse_slot(line);
		break;

	case 's':	/* slot ignored */
	case 'c':	/* card ignored */
	default:	/* ignored */
		break;

	}
}


int	main(int argc, char *argv[])
{

	char line[65536];	/* fixme */

	while (!feof(stdin)) {
		fgets(line, sizeof(line), stdin);
		parse_input(line);
	}

	exit(0);
}

