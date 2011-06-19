
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <getopt.h>
#include <alloca.h>
#include <ctype.h>

#include <stdint.h>
#include <stdbool.h>
#include <math.h>

#include <SDL/SDL.h>
#include <SDL/SDL_ttf.h>
#include <SDL/SDL_gfxPrimitives.h>
#include <SDL/SDL_video.h>

extern int usleep(unsigned long usec);


#define	MIN(a,b)	(((a) < (b)) ? (a) : (b))
#define	MAX(a,b)	(((a) > (b)) ? (a) : (b))

#define	CLAMP(v,a,b)	MIN(b, MAX(a,v))


#define	VID_WIDTH	1240
#define	VID_HEIGHT	600

#define SLOT_WIDTH	32
#define	SLOT_HEIGHT	32

#define	CARD_HEIGHT	40

#define	VITA_WIDTH	12

#define	PLAY_WIDTH	((SLOT_WIDTH * 16) + 1)
#define	PLAY_HEIGHT	((SLOT_HEIGHT * 16) + 1)


#define	STAT_WIDTH	80
#define	STAT_HEIGHT	48

#define	SCORE_WIDTH	128
#define	SCORE_HEIGHT	38

#define	FONT_HEIGHT	10

#define	START_VITALITY	10000
#define	BASE_VITALITY	(START_VITALITY * 256)


void	str_free(char *str)
{
	free(str);
}

char *	str_copy(char *str)
{
	unsigned len = strlen(str);
	char *copy = malloc(len + 1);

	if (copy)
		memcpy(copy, str, len + 1);
	return copy;
}


void	hsv2rgb(unsigned h, unsigned s, unsigned v,
	unsigned *r, unsigned *g, unsigned *b)
{
	if (s == 0) {
		*r = *g = *b = v;
	} else {
		float hh = (float)h / 60;
		float ss = (float)s / 255;
		int i = floor(hh);
		float f = hh - i;
		float p = v * (1 - ss);
		float q = v * (1 - ss * f);
		float t = v * (1 - ss * (1 - f));

		switch (i) {
		default:
			*r = v; *g = p; *b = q;
			break;
		case 0:
			*r = v; *g = t; *b = p;
			break;
		case 1:
			*r = q; *g = v; *b = p;
			break;
		case 2:
			*r = p; *g = v; *b = t;
			break;
		case 3:
			*r = p; *g = q; *b = v;
			break;
		case 4:
			*r = t; *g = p; *b = v;
			break;
		}
	}
}


#define	TI_IDLE		50



#define	FL_TOUCHED	1
#define	FL_HURT		2
#define	FL_HEALED	4
#define	FL_RESET	8
#define	FL_CHANGED	16


#define	TI_VITALITY	20
#define	TI_CHANGED	16
#define	TI_RESET	8

typedef	char vtim_t;

typedef	struct _slot {
	int64_t field;
	int64_t vitality;
	char *seq;
	unsigned flags;
	struct _vtim {
		vtim_t ti_vitality;
		vtim_t ti_changed;
		vtim_t ti_reset;
	} vtim;
}	slot_t;

slot_t	player[2][256];

typedef	struct _stat {
	int64_t total_vitality;
	int64_t slots_alive;
	int64_t zombies;
	int64_t exceptions;
}	stat_t;

stat_t	player_stat[2];


vtim_t	ti_idle;


bool	is_slot_dead(slot_t *slot)
{
	if (slot->vitality == 0)
		return true;
	if (slot->vitality == -1)
		return true;
	return false;
}

bool	is_slot_integer(slot_t *slot)
{
	if (slot->field >= 0)
		return true;
	return false;
}

bool	is_slot_index(slot_t *slot)
{
	if ((slot->field >= 0) &&
	    (slot->field < 256))
		return true;
	return false;
}

#define	FL_LEFT		1024
#define	FL_RIGHT	2048

#define	TI_APPLIED	16

typedef	struct _card {
	char *name;
	unsigned flags;
	vtim_t ti_applied;
	int64_t left;
	int64_t right;
}	card_t;

card_t	cards[2][16] = {{
	{ .name = "I" },
	{ .name = "zero" },
	{ .name = "succ" },
	{ .name = "dbl" },
	{ .name = "get" },
	{ .name = "put" },
	{ .name = "S" },
	{ .name = "K" },
	{ .name = "inc" },
	{ .name = "dec" },
	{ .name = "attack" },
	{ .name = "help" },
	{ .name = "copy" },
	{ .name = "revive" },
	{ .name = "zombie" },
	{ .name = "XXX" }}, { }};


static  char *	cmd_name;
char	opt_brief = 0;
char	opt_quiet = 0;
char	opt_single = 0;
uint64_t	opt_halt = ~0;


card_t * card_from_name(unsigned play_id, char *name)
{
	for (int i = 0; i < 16; i++)
		if (strcmp(cards[play_id][i].name, name) == 0)
			return &cards[play_id][i];
	return NULL;
}



void	vis_init_slots(slot_t slot[256])
{
	for (int i=0; i<256; i++) {
		slot[i].field = -1;
		slot[i].vitality = 10000;
		slot[i].seq = str_copy("I");
		slot[i].flags = 0;
	}
}

void	vis_calc_stats(stat_t *stat, slot_t slot[256])
{
	stat->slots_alive = 0;
	stat->total_vitality = 0;
	stat->zombies = 0;
	
	for (int i=0; i<256; i++) {
		if (slot[i].vitality > 0) {
			stat->slots_alive++;
			stat->total_vitality += slot[i].vitality;
		} else if (slot[i].vitality < 0)
			stat->zombies++;
	}
}


bool	vis_apply_card_to_slot(int play_id, int card_id, int slot_id)
{
	switch (card_id) {
	case -1:	/* I x */
		break;
	
	case -2:	/* zero */
		break;
	
	case -3:	/* succ n*/
		break;
	
	case -4:	/* dbl n */
		break;
	
	case -5:	/* get i */
		break;
	
	case -6:	/* put x y */
		break;
	case -7:	/* S f g x */
		break;
	case -8:	/* K x y */
		break;
	case -9:	/* inc i */
		break;
	case -10:	/* dec i */
		break;
	case -11:	/* attack i j n */
		break;
	case -12:	/* help i j n */
		break;
	case -13:	/* copy i */
		break;
	case -14:	/* revive i */
		break;
	case -15:	/* zombie i x */
		break;
	default:
		/* error case */
		goto out_err;
	}
	return true;

out_err:
	return false;
}

bool	vis_apply_slot_to_card(int play_id, int slot_id, int card_id)
{
	switch (card_id) {
	case -1:	/* I x */
		break;
	
	case -2:	/* zero */
		break;
	
	case -3:	/* succ n*/
		break;
	
	case -4:	/* dbl n */
		break;
	
	case -5:	/* get i */
		break;
	
	case -6:	/* put x y */
		break;
	case -7:	/* S f g x */
		break;
	case -8:	/* K x y */
		break;
	case -9:	/* inc i */
		break;
	case -10:	/* dec i */
		break;
	case -11:	/* attack i j n */
		break;
	case -12:	/* help i j n */
		break;
	case -13:	/* copy i */
		break;
	case -14:	/* revive i */
		break;
	case -15:	/* zombie i x */
		break;
	default:
		/* error case */
		goto out_err;
	}
	return true;

out_err:
	return false;
}



TTF_Font *small_fnt = NULL;
TTF_Font *stats_fnt = NULL;
TTF_Font *score_fnt = NULL;
TTF_Font *win_fnt = NULL;


SDL_Surface *logo = NULL;

uint64_t turn = 0;
int play_id = 0;
int card_id = 0;
int slot_id = 0;
int vitality = 0;

int player_win = 0;

#define	PLAYER_WIN_A	1
#define	PLAYER_WIN_B	2
#define	PLAYER_DRAW	3


void	vis_draw_grid(SDL_Surface *dst, unsigned slotw, unsigned sloth)
{
	for (int x = 0; x < 17; x++)
		vlineColor(dst, x * SLOT_WIDTH, 0, PLAY_HEIGHT, 0x8F8F8FFF);
	for (int y = 0; y < 17; y++)
		hlineColor(dst, 0, PLAY_WIDTH, y * SLOT_HEIGHT, 0x8F8F8FFF);
}

void	vis_draw_card_grid(SDL_Surface *dst, unsigned slotw, unsigned sloth)
{
	for (int x = 0; x < 17; x++)
		vlineColor(dst, x * SLOT_WIDTH, 0, CARD_HEIGHT, 0x8F8F8FFF);
	for (int y = 0; y < 2; y++)
		hlineColor(dst, 0, PLAY_WIDTH, y * CARD_HEIGHT, 0x8F8F8FFF);
}


void	vis_draw_string(SDL_Surface *dst, unsigned xp, unsigned yp,
		TTF_Font *fnt, char *str, Uint8 r, Uint8 g, Uint8 b)
{
	SDL_Rect dstRect = {xp, yp, 0, 0};
	SDL_Color fg = { .r = r, .g = g, .b = b };
#if 0
	SDL_Color bg = { .r = 0, .g = 0, .b = 0 };
	
	SDL_Surface *sText = TTF_RenderText_Shaded(fnt, str, fg, bg);
#else
	SDL_Surface *sText = TTF_RenderText_Solid(fnt, str, fg);
#endif
	SDL_BlitSurface(sText, NULL, dst, &dstRect);
	SDL_FreeSurface(sText);
}

void	vis_draw_string_shaded(SDL_Surface *dst, unsigned xp, unsigned yp,
		TTF_Font *fnt, char *str,
		Uint8 r, Uint8 g, Uint8 b,
		Uint8 rb, Uint8 gb, Uint8 bb)
{
	SDL_Rect dstRect = {xp, yp, 0, 0};
	SDL_Color fg = { .r = r, .g = g, .b = b };
	SDL_Color bg = { .r = rb, .g = gb, .b = bb };
	
	SDL_Surface *sText = TTF_RenderText_Shaded(fnt, str, fg, bg);
	SDL_BlitSurface(sText, NULL, dst, &dstRect);
	SDL_FreeSurface(sText);
}

void	vis_draw_string_center(SDL_Surface *dst, unsigned xp, unsigned yp,
		TTF_Font *fnt, char *str, Uint8 r, Uint8 g, Uint8 b)
{
	int w, h;
	
	TTF_SizeText(fnt, str, &w, &h);
	
	vis_draw_string(dst, xp - w / 2, yp, fnt, str, r, g, b);
}

void	vis_draw_string_center_shaded(SDL_Surface *dst, unsigned xp, unsigned yp,
		TTF_Font *fnt, char *str,
		Uint8 r, Uint8 g, Uint8 b,
		Uint8 rb, Uint8 gb, Uint8 bb)
{
	int w, h;
	
	TTF_SizeText(fnt, str, &w, &h);
	
	vis_draw_string_shaded(dst, xp - w / 2, yp, fnt, str,
		r, g, b, rb, gb, bb);
}

void	vis_slot_background(slot_t *slot,
	unsigned *r, unsigned *g, unsigned *b)
{
	*r = 15;
	*g = 15;
	*b = 15;
}


void	vis_timer_set(vtim_t *t, unsigned val)
{
	*t = val;
}

int	vis_timer(vtim_t *t)
{
	vtim_t cur = *t;

	if (cur)
		(*t)--;
	return cur;
}

int	vis_mix(float mix, unsigned a, unsigned b)
{
	float val = a * mix + b * (1 - mix);
	return (int)val;
}

void	vis_draw_slot_vitality(SDL_Surface *dst,
		unsigned xp, unsigned yp, uint64_t vitality)
{
	float val = MIN((float)vitality / START_VITALITY, 2.0);

	unsigned width = val * (SLOT_WIDTH - 6) / 2;

	unsigned r, g, b;

	hsv2rgb(val * 90, 255, 255, &r, &g, &b);

	boxRGBA(dst, xp + 3, yp + SLOT_HEIGHT - 8,
		xp + 3 + width, yp + SLOT_HEIGHT - 6, r, g, b, 128);
}

void	vis_draw_slot(SDL_Surface *dst, unsigned x, unsigned y, slot_t *slot)
{
	unsigned xp = x * SLOT_WIDTH + 1;
	unsigned yp = y * SLOT_HEIGHT + 1;

	unsigned r, g, b;

	vis_slot_background(slot, &r, &g, &b);

	if (slot->flags & FL_CHANGED) {
		unsigned tv = vis_timer(&slot->vtim.ti_changed);
		
		if (tv)
			b = vis_mix((float)tv/TI_CHANGED, 255, b);
		else
			slot->flags &= ~FL_CHANGED;
	}
	
	if (slot->flags & FL_HURT) {
		unsigned tv = vis_timer(&slot->vtim.ti_vitality);
		
		if (tv)
			r = vis_mix((float)tv/TI_VITALITY, 255, r);
		else
			slot->flags &= ~FL_HURT;
	}
	if (slot->flags & FL_HEALED) {
		unsigned tv = vis_timer(&slot->vtim.ti_vitality);
		
		if (tv)
			g = vis_mix((float)tv/TI_VITALITY, 255, g);
		else
			slot->flags &= ~FL_HEALED;
	}
	if (slot->flags & FL_RESET) {
		unsigned tv = vis_timer(&slot->vtim.ti_reset);
		
		if (tv) {
			r = vis_mix((float)tv/TI_RESET, 255, r);
			g = vis_mix((float)tv/TI_RESET, 255, g);
			b = vis_mix((float)tv/TI_RESET, 255, b);
		} else
			slot->flags &= ~FL_RESET;
	}

	boxRGBA(dst, xp, yp, xp + SLOT_WIDTH - 2, yp + SLOT_HEIGHT - 2,
		r, g, b, 255);

	char line[3][64] =  { "", "" };

	if (slot->seq) {
		int ssl = strlen(slot->seq);
		
		if (ssl <= 7)
			sprintf(line[0], "%7.7s", slot->seq);
		else
			sprintf(line[0], "{%d}", ssl);
	} else {
		sprintf(line[0], "%6ld", slot->field);
	}

	sprintf(line[1], "%5ld", slot->vitality);

	for (int i=0; i<2; i++)
		vis_draw_string_center(dst, xp + SLOT_WIDTH / 2, yp + 2 + i * FONT_HEIGHT,
			small_fnt, line[i], 255, 255, 255);

	vis_draw_slot_vitality(dst, xp, yp, slot->vitality);
}

void	vis_draw_slots(SDL_Surface *dst, slot_t slot[256])
{
	for (int x = 0; x < 16; x++) {
		for (int y = 0; y < 16; y++) {
			vis_draw_slot(dst, x, y, &slot[x + y*16]);
		}
	}
}

void	vis_draw_card(SDL_Surface *dst, unsigned x, card_t *card)
{
	unsigned xp = x * SLOT_WIDTH + 1;
	unsigned yp = 1;

	unsigned r = 64;
	unsigned g = 64;
	unsigned b = 64;

	if (card->flags & FL_LEFT) {
		unsigned tv = vis_timer(&card->ti_applied);
		
		if (tv)
			r = vis_mix((float)tv/TI_APPLIED, 255, r);
		else
			card->flags &= ~FL_LEFT;
	}
	if (card->flags & FL_RIGHT) {
		unsigned tv = vis_timer(&card->ti_applied);
		
		if (tv)
			g = vis_mix((float)tv/TI_APPLIED, 255, g);
		else
			card->flags &= ~FL_RIGHT;
	}

	boxRGBA(dst, xp, yp, xp + SLOT_WIDTH - 2, yp + CARD_HEIGHT - 2,
		r, g, b, 255);

	char line[3][64] =  { "", "" };

	if (card->name)
		sprintf(line[0], "%s", card->name);

	sprintf(line[1], "%6ld", card->left);
	sprintf(line[2], "%6ld", card->right);

	for (int i=0; i<3; i++)
		vis_draw_string_center(dst, xp + SLOT_WIDTH / 2, yp + 2 + i * FONT_HEIGHT,
			small_fnt, line[i], 255, 255, 255);
}

void	vis_draw_cards(SDL_Surface *dst, unsigned play_id)
{
	for (int i = 0; i < 16; i++) {
		vis_draw_card(dst, i, &cards[play_id][i]);
	}
}


void	vis_draw_vitality(SDL_Surface *dst, uint64_t vitality)
{
	float val = MIN((float)vitality / BASE_VITALITY, 2.0);

	unsigned height = val * PLAY_HEIGHT / 2;
	unsigned hinv = PLAY_HEIGHT - height;

	unsigned r, g, b;

	hsv2rgb(val * 90, 255, 255, &r, &g, &b);

	boxRGBA(dst, 0, 0, VITA_WIDTH, hinv, 64, 64, 64, 255);
	boxRGBA(dst, 0, hinv, VITA_WIDTH, hinv + height, r, g, b, 255);
}

void	vis_draw_stats(SDL_Surface *dst, stat_t *stat)
{
	boxRGBA(dst, 0, 0, STAT_WIDTH, STAT_HEIGHT, 32, 32, 32, 255);

	char line[5][64] =  { "", "", "", "", "" };

	sprintf(line[0], "vitavg: %ld", stat->total_vitality / 256);
	sprintf(line[1], "zombies: %ld", stat->zombies);
	sprintf(line[2], "xcept: %ld", stat->exceptions);

	for (int i=0; i<3; i++)
		vis_draw_string_shaded(dst, 4, 4 + i * 14,
			stats_fnt, line[i], 255, 255, 255, 32, 32, 32);
}

void	vis_draw_score(SDL_Surface *dst)
{
	boxRGBA(dst, 0, 0, SCORE_WIDTH, SCORE_HEIGHT, 64, 64, 64, 255);
	
	char line[2][64] =  { "", "" };

	sprintf(line[0], "%ld", turn);
	sprintf(line[1], "%ld : %ld",
		player_stat[0].slots_alive, player_stat[1].slots_alive);

	for (int i=0; i<2; i++)
		vis_draw_string_center_shaded(dst, SCORE_WIDTH / 2, 2 + i * 16,
			score_fnt, line[i], 255, 255, 255, 64, 64, 64);
}


void	vis_draw_win(SDL_Surface *dst0, SDL_Surface *dst1)
{
	char *win_txt = "WIN!";
	char *lose_txt = "LOSE!";
	char *draw_txt = "DRAW!";

	switch (player_win) {
	case PLAYER_WIN_A:
		vis_draw_string_center_shaded(dst0, PLAY_WIDTH / 2, PLAY_HEIGHT / 2,
			win_fnt, win_txt, 0, 255, 0, 0, 0, 0);
		vis_draw_string_center_shaded(dst1, PLAY_WIDTH / 2, PLAY_HEIGHT / 2,
			win_fnt, lose_txt, 255, 0, 0, 0, 0, 0);
		break;
	case PLAYER_WIN_B:
		vis_draw_string_center_shaded(dst1, PLAY_WIDTH / 2, PLAY_HEIGHT / 2,
			win_fnt, win_txt, 0, 255, 0, 0, 0, 0);
		vis_draw_string_center_shaded(dst0, PLAY_WIDTH / 2, PLAY_HEIGHT / 2,
			win_fnt, lose_txt, 255, 0, 0, 0, 0, 0);
		break;
	case PLAYER_DRAW:
		vis_draw_string_center_shaded(dst0, PLAY_WIDTH / 2, PLAY_HEIGHT / 2,
			win_fnt, draw_txt, 255, 255, 0, 0, 0, 0);
		vis_draw_string_center_shaded(dst1, PLAY_WIDTH / 2, PLAY_HEIGHT / 2,
			win_fnt, draw_txt, 255, 255, 0, 0, 0, 0);
		break;
	} 
}



bool	parse_turn_info(char *line)
{
	sscanf(line, "###### turn %ld", &turn);
	printf("\nturn %ld\n", turn);
	
	while (turn >= opt_halt)
		usleep(100000);

	return true;
}

bool	parse_player_info(char *line)
{
	char card_name[64];
	int n;

	n = sscanf(line, "player %d applied card %s to slot %d",
		&play_id, card_name, &slot_id);
	if (n == 3) {
		if (!opt_quiet)
			printf("%c: %s -> [%d]\n",
				play_id ? 'B' : 'A',
				card_name, slot_id);
			
		card_t *card = card_from_name(play_id, card_name);
		if (card) {
			card->flags |= FL_LEFT;
			vis_timer_set(&card->ti_applied, TI_APPLIED);
			card->left++;
		}
		return true;
	}

	n = sscanf(line, "player %d applied slot %d to card %s",
		&play_id, &slot_id, card_name);
	if (n == 3) {
		if (!opt_quiet)
			printf("%c: [%d] -> %s\n",
				play_id ? 'B' : 'A',
				slot_id, card_name);

		card_t *card = card_from_name(play_id, card_name);
		if (card) {
			card->flags |= FL_RIGHT;
			vis_timer_set(&card->ti_applied, TI_APPLIED);
			card->right++;
		}
		return true;
	}
	return false;
}

bool	parse_player_turn(char *line)
{
	sscanf(line, "*** player %d'", &play_id);
	if (!opt_quiet)
		printf("player %c\n", play_id ? 'B' : 'A');
	return false;
}

bool	parse_slot(char *line)
{
	static char expr[65536];
	int n = sscanf(line, "%d={%d,%[^}]", &slot_id, &vitality, expr);
	if (n == 3) {
		slot_t *slot =  &player[play_id][slot_id];

		if (!opt_brief && !opt_quiet)
			printf("[%d]: |%d| %s\n", slot_id, vitality, expr);

		if (slot->vitality < vitality) {
			slot->vitality = vitality;
			slot->flags |= FL_HEALED;
			vis_timer_set(&slot->vtim.ti_vitality, TI_VITALITY);
		} else if (slot->vitality > vitality) {
			slot->vitality = vitality;
			slot->flags |= FL_HURT;
			vis_timer_set(&slot->vtim.ti_vitality, TI_VITALITY);
		}
		
		if (isdigit(expr[0])) {
			unsigned val = atol(expr);

			if (player[play_id][slot_id].field == val)
				return false;

			str_free(player[play_id][slot_id].seq);
			player[play_id][slot_id].field = val;
			player[play_id][slot_id].seq = NULL;
			slot->flags |= FL_CHANGED;
			vis_timer_set(&slot->vtim.ti_changed, TI_CHANGED);
		} else  {
			if (player[play_id][slot_id].seq &&
				(strcmp(player[play_id][slot_id].seq,
					expr) == 0))
				return false;

			str_free(player[play_id][slot_id].seq);
			player[play_id][slot_id].field = -1;
			player[play_id][slot_id].seq = str_copy(expr);
			slot->flags |= FL_CHANGED;
			vis_timer_set(&slot->vtim.ti_changed, TI_CHANGED);
		}
	}
	return true;
}

bool	parse_slot_reset(char *line)
{
	int n = sscanf(line, "slot %d reset to I", &slot_id);
	if (n == 1) {
		slot_t *slot =  &player[play_id][slot_id];

		if (!opt_quiet)
			printf("slot %d reset\n", slot_id);
		slot->flags |= FL_RESET;
		vis_timer_set(&slot->vtim.ti_reset, TI_RESET);
	}
	return true;
}

bool	parse_expr(char *line)
{
	/* (slots ... ) and (1) ignored for now */
	return false;
}

bool	parse_exception(char *line)
{
	if (!opt_quiet)
		printf("%s\n", line);
	player_stat[play_id].exceptions++;
	cards[play_id][15].flags |= FL_LEFT;
	cards[play_id][15].ti_applied = TI_APPLIED;
	return true;
}

bool	parse_win(char *line)
{
	int n;
	unsigned score0, score1;

	n = sscanf(line, "!! player %d wins by %d:%d after turn %ld",
		&play_id, &score0, &score1, &turn);
	if (n == 4) {
		if (!opt_quiet)
			printf("!! player %c wins by %d:%d after turn %ld\n",
				play_id ? 'B' : 'A',
				score0, score1, turn);
		player_win = play_id ? PLAYER_WIN_B : PLAYER_WIN_A;
		return true;
	}
	n = sscanf(line, "!! draw by %d:%d after turn %ld",
		&score0, &score1, &turn);
	if (n == 3) {
		if (!opt_quiet)
			printf("!! draw by %d:%d after turn %ld\n",
				score0, score1, turn);
		player_win = PLAYER_DRAW;
		return true;
	}
	n = sscanf(line, "!! player %d loses by invalid output at turn %ld",
		&play_id, &turn);
	if (n == 2) {
		if (!opt_quiet)
			printf("!! player %c loses by invalid output at turn %ld\n",
				play_id ? 'B' : 'A', turn);
		player_win = play_id ? PLAYER_WIN_B : PLAYER_WIN_A;
		return true;
	}
	return true;
}

enum	_state {
	ST_INIT	= 0,
	ST_SCAN	= 1,
	ST_TURN = 2,
};


bool	parse_input(char *line)
{
	static char state = ST_INIT;
	bool ret = false;

	switch (line[0]) {
	case 'L':	/* Lambda */
		state = ST_SCAN;
		break;

	case '#':	/* turn info */
		ret = parse_turn_info(line);
		break;

	case 'p':	/* player info */
		ret = parse_player_info(line);
		break;

	case '*':	/* players turn */
		ret = parse_player_turn(line);
		state = ST_TURN;
		break;

	case '(':	/* refine */
		ret = parse_expr(line);
		state = ST_SCAN;
		break;

	case 'E':	/* exception */
		ret = parse_exception(line);
		break;

	case 's':	/* slot reset */
		ret = parse_slot_reset(line);
		break;

	case '!':	/* win */
		ret = parse_win(line);
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
		ret = parse_slot(line);
		break;

	case '%':	/* debug info */
		fprintf(stderr, "%s", line);
		break;
	case 'c':	/* card ignored */
	default:	/* ignored */
		break;
	}
	return ret;
}




int	do_parse(void *dummy)
{
	static char line[65536];	/* fixme */
	bool ret;

	while (!feof(stdin)) {
		fgets(line, sizeof(line), stdin);
		ret = parse_input(line);
		if (ret)
			vis_timer_set(&ti_idle, TI_IDLE);
	}
	return 0;
}


#define	VERSION		"0.11"

#define	OPTIONS		"hbqsn:"


int	main(int argc, char *argv[])
{
	extern int optind;
	extern char *optarg;
	char c, errflg = 0;

	cmd_name = argv[0];
	while ((c = getopt(argc, argv, OPTIONS)) != EOF) {
	    switch (c) {
	    case 'h':
		fprintf(stderr,
		    "This is %s " VERSION "\n"
		    "options are:\n"
		    "-h        print this help message\n"
		    "-b        brief output (don't show slots)\n"
		    "-q        quiet (don't show anything)\n"
		    "-s        single step\n"
		    "-n <num>  halt at turn <num>\n"
		    , cmd_name);
		exit(0);
		break;
	    case 'b':
		opt_brief = 1;
		break;
	    case 'q':
		opt_quiet = 1;
		break;
	    case 's':
		opt_single = 1;
		break;
	    case 'n':
		opt_halt = atol(optarg);
		break;
	    case '?':
	    default:
		errflg++;
		break;
	    }
	}
	if (errflg) {
	    fprintf(stderr, 
		"Usage: %s -[" OPTIONS "] ...\n"
		"%s -h for help.\n",
		cmd_name, cmd_name);
	    exit(2);
	}

	SDL_Surface *screen;

	if (SDL_Init(SDL_INIT_VIDEO) != 0) {
		fprintf(stderr,
			"Unable to initialize SDL: %s\n",
			SDL_GetError());
		exit(1);
	}
	atexit(SDL_Quit);

	screen = SDL_SetVideoMode(VID_WIDTH, VID_HEIGHT, 16,
		SDL_DOUBLEBUF | SDL_SWSURFACE);

	SDL_Surface *play0, *play1;
	play0 = SDL_CreateRGBSurface(SDL_SWSURFACE,
		PLAY_WIDTH, PLAY_HEIGHT, 32, 0, 0, 0, 0);
	play1 = SDL_CreateRGBSurface(SDL_SWSURFACE,
		PLAY_WIDTH, PLAY_HEIGHT, 32, 0, 0, 0, 0);

	SDL_Surface *card0, *card1;
	card0 = SDL_CreateRGBSurface(SDL_SWSURFACE,
		PLAY_WIDTH, CARD_HEIGHT + 1, 32, 0, 0, 0, 0);
	card1 = SDL_CreateRGBSurface(SDL_SWSURFACE,
		PLAY_WIDTH, CARD_HEIGHT + 1, 32, 0, 0, 0, 0);

	SDL_Surface *vita0, *vita1;
	vita0 = SDL_CreateRGBSurface(SDL_SWSURFACE,
		VITA_WIDTH, PLAY_HEIGHT, 32, 0, 0, 0, 0);
	vita1 = SDL_CreateRGBSurface(SDL_SWSURFACE,
		VITA_WIDTH, PLAY_HEIGHT, 32, 0, 0, 0, 0);

	SDL_Surface *stat0, *stat1, *score;
	stat0 = SDL_CreateRGBSurface(SDL_SWSURFACE,
		STAT_WIDTH, STAT_HEIGHT, 32, 0, 0, 0, 0);
	stat1 = SDL_CreateRGBSurface(SDL_SWSURFACE,
		STAT_WIDTH, STAT_HEIGHT, 32, 0, 0, 0, 0);
	score = SDL_CreateRGBSurface(SDL_SWSURFACE,
		SCORE_WIDTH, SCORE_HEIGHT, 32, 0, 0, 0, 0);


	SDL_WM_SetCaption("LTG Sim", "LTG Sim");

	if (TTF_Init() != 0) {
		fprintf(stderr,
			"Unable to initialize TTF: %s\n",
			SDL_GetError());
		exit(2);
	}

	small_fnt = TTF_OpenFont("/icfpnfs/BERTL/vis_small.ttf", 8);
	if (!small_fnt) {
		fprintf(stderr,
			"Unable to open font: %s\n",
			SDL_GetError());
		exit(3);
	}

	stats_fnt = TTF_OpenFont("/icfpnfs/BERTL/vis_stats.ttf", 9);
	if (!stats_fnt) {
		fprintf(stderr,
			"Unable to open font: %s\n",
			SDL_GetError());
		exit(4);
	}

	score_fnt = TTF_OpenFont("/icfpnfs/BERTL/vis_score.ttf", 15);
	if (!score_fnt) {
		fprintf(stderr,
			"Unable to open font: %s\n",
			SDL_GetError());
		exit(5);
	}

	win_fnt = TTF_OpenFont("/icfpnfs/BERTL/vis_win.ttf", 64);
	if (!win_fnt) {
		fprintf(stderr,
			"Unable to open font: %s\n",
			SDL_GetError());
		exit(6);
	}

	logo = SDL_DisplayFormat(SDL_LoadBMP("/icfpnfs/BERTL/vis_logo.bmp"));
	if (!logo) {
		fprintf(stderr,
			"Unable to open image: %s\n",
			SDL_GetError());
		exit(7);
	}

	memcpy(cards[1], cards[0], sizeof(cards[0]));

	vis_init_slots(player[0]);
	vis_init_slots(player[1]);

	vis_draw_grid(play0, SLOT_WIDTH, SLOT_HEIGHT);
	vis_draw_grid(play1, SLOT_WIDTH, SLOT_HEIGHT);

	vis_calc_stats(&player_stat[0], player[0]);
	vis_draw_slots(play0, player[0]);

	vis_calc_stats(&player_stat[1], player[1]);
	vis_draw_slots(play1, player[1]);

	vis_draw_card_grid(card0, SLOT_WIDTH, CARD_HEIGHT);
	vis_draw_card_grid(card1, SLOT_WIDTH, CARD_HEIGHT);

	vis_draw_cards(card0, 0);
	vis_draw_cards(card1, 1);

	vis_draw_vitality(vita0, player_stat[0].total_vitality);
	vis_draw_vitality(vita1, player_stat[1].total_vitality);

	vis_draw_score(score);
	vis_draw_stats(stat0, &player_stat[0]);
	vis_draw_stats(stat1, &player_stat[1]);

	SDL_Rect logoRect = { (VID_WIDTH - logo->w) / 2, (PLAY_HEIGHT + 20 - logo->h) / 2, logo->w, logo->h };
	SDL_BlitSurface(logo, NULL, screen, &logoRect);

	SDL_Thread *parse_thread = SDL_CreateThread(do_parse, NULL);

	unsigned frame = 0;

	while (1) {
		unsigned itv = vis_timer(&ti_idle);

		if (!itv)
			goto skip_redraw;

		vis_calc_stats(&player_stat[0], player[0]);
		vis_draw_slots(play0, player[0]);

		vis_calc_stats(&player_stat[1], player[1]);
		vis_draw_slots(play1, player[1]);

		vis_draw_win(play0, play1);

		SDL_Rect psrcRect = { 0, 0, PLAY_WIDTH, PLAY_HEIGHT };
		SDL_Rect play0Rect = { 10, 10, PLAY_WIDTH, PLAY_HEIGHT };
		SDL_Rect play1Rect = { VID_WIDTH - 10 - PLAY_WIDTH, 10, PLAY_WIDTH, PLAY_HEIGHT };

		SDL_BlitSurface(play0, &psrcRect, screen, &play0Rect);
		SDL_BlitSurface(play1, &psrcRect, screen, &play1Rect);

		vis_draw_cards(card0, 0);
		vis_draw_cards(card1, 1);

		SDL_Rect csrcRect = { 0, 0, PLAY_WIDTH, CARD_HEIGHT + 1 };
		SDL_Rect card0Rect = { 10, 20 + PLAY_HEIGHT, PLAY_WIDTH, CARD_HEIGHT + 1 };
		SDL_Rect card1Rect = { VID_WIDTH - 10 - PLAY_WIDTH, 20 + PLAY_HEIGHT, PLAY_WIDTH, CARD_HEIGHT + 1 };

		SDL_BlitSurface(card0, &csrcRect, screen, &card0Rect);
		SDL_BlitSurface(card1, &csrcRect, screen, &card1Rect);

		vis_draw_vitality(vita0, player_stat[0].total_vitality);
		vis_draw_vitality(vita1, player_stat[1].total_vitality);

		SDL_Rect vsrcRect = { 0, 0, VITA_WIDTH, PLAY_HEIGHT };
		SDL_Rect vita0Rect = { 20 + PLAY_WIDTH, 10, VITA_WIDTH, PLAY_HEIGHT };
		SDL_Rect vita1Rect = { VID_WIDTH - 20 - PLAY_WIDTH - VITA_WIDTH, 10, VITA_WIDTH, PLAY_HEIGHT };

		SDL_BlitSurface(vita0, &vsrcRect, screen, &vita0Rect);
		SDL_BlitSurface(vita1, &vsrcRect, screen, &vita1Rect);

		vis_draw_score(score);
		vis_draw_stats(stat0, &player_stat[0]);
		vis_draw_stats(stat1, &player_stat[1]);

		SDL_Rect ssrcRect = { 0, 0, STAT_WIDTH, STAT_HEIGHT };
		SDL_Rect stat0Rect = { 30 + PLAY_WIDTH + VITA_WIDTH, 10, STAT_WIDTH, STAT_HEIGHT };
		SDL_Rect stat1Rect = { VID_WIDTH - 30 - PLAY_WIDTH - VITA_WIDTH - STAT_WIDTH,
			10 + PLAY_HEIGHT - STAT_HEIGHT, STAT_WIDTH, STAT_HEIGHT };

		SDL_BlitSurface(stat0, &ssrcRect, screen, &stat0Rect);
		SDL_BlitSurface(stat1, &ssrcRect, screen, &stat1Rect);

		SDL_Rect xsrcRect = { 0, 0, SCORE_WIDTH, SCORE_HEIGHT };
		SDL_Rect scoreRect = { ( VID_WIDTH - SCORE_WIDTH ) / 2, 
			20 + PLAY_HEIGHT + CARD_HEIGHT - SCORE_HEIGHT, SCORE_WIDTH, SCORE_HEIGHT };

		SDL_BlitSurface(score, &xsrcRect, screen, &scoreRect);

		SDL_Flip(screen); //Refresh the screen

	skip_redraw:;
		SDL_Event event; /* Event structure */
		if (SDL_PollEvent(&event)) {
			if (event.type == SDL_QUIT)
				break;
			if (event.type == SDL_KEYDOWN) {
			
				switch(event.key.keysym.sym){
				case SDLK_SPACE:
					opt_halt++;
					break;
				case SDLK_ESCAPE:
					exit(0);
					break;
				case SDLK_1:
					opt_halt += 2;
					break;
				case SDLK_2:
					opt_halt += 4;
					break;
				case SDLK_3:
					opt_halt += 8;
					break;
				case SDLK_4:
					opt_halt += 16;
					break;
				case SDLK_5:
					opt_halt += 32;
					break;
				case SDLK_6:
					opt_halt += 64;
					break;
				case SDLK_7:
					opt_halt += 128;
					break;
				case SDLK_8:
					opt_halt += 256;
					break;
				case SDLK_9:
					opt_halt += 512;
					break;
				case SDLK_0:
					opt_halt += 1024;
					break;
				default:
					break;
				}
			}
		}

		usleep(10000);
		frame++;
	}

	SDL_KillThread(parse_thread);

	exit(0);
}

