
all:	ltg_vis ltg_clean

ltg_vis :	ltg_vis.c
	$(CC) -Wall -std=c99 -O9 -lSDL -lSDL_ttf -lSDL_gfx -o $@ $^

ltg_clean :	ltg_clean.c
	$(CC) -Wall -std=c99 -O9 -o $@ $^

ltg_vis.debug : ltg_vis.c
	$(CC) -Wall -g -ggdb -gstabs -std=c99 -O0 -lSDL -lSDL_ttf -lSDL_gfx -o $@ $^

clean:
	$(RM) ltg_vis ltg_vis.debug ltg_clean
	
